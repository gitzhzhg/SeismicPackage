
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
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ pc.f90 --------------------------------!!
!!------------------------------ pc.f90 --------------------------------!!
!!------------------------------ pc.f90 --------------------------------!!





!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : PC
! Category   : character
! Written    : 1999-06-25   by: Tom Stoeckley
! Revised    : 2008-12-11   by: Bill Menger
! Maturity   : beta
! Purpose    : Parameter cache for passing parameters to/from process modules.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>



!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This module is a "parameter cache" which provides a generic method of
! communicating between a process module and its calling program.  Its
! calling program can be a processing system (front-end or back-end), another
! process module which calls it internally, or any other program (such
! as a workstation program or an interactive processing system).
!
! This parameter cache is ignorant of the nature of individual process
! modules and processing systems.  It contains certain parameters needed
! by process modules or processing systems, and subroutines for accessing
! the parameters (putting them into the cache or getting them out of the
! cache).
!
! There are two ways to transfer parameters into and out of the parameter
! cache.  One way is by using data cards.  The other way is by accessing
! parameters individually by keyword.  There are also some convenience
! subroutines for accessing certain parameters without specifying their
! keywords.
!
! Data cards are card images which contain parameter values identified by
! keywords.  Data cards can be used as a generic method of communication
! within the processing system.  For example, they can be transferred to and
! from workfiles and jobfiles.
!
! Data cards transferred into the parameter cache can be in either packed
! or unpacked format. The data cards transferred out of the parameter cache
! are always in packed format.  The format of the data cards is documented
! in the CARDSET primitive and will not be repeated here.
! 
!-------------------------------------------------------------------------------
!          WHEN TO USE THE PARAMETER CACHE FROM A PROCESSING MODULE
!
! The parameter cache is intended primarily for use at the front end (when
! process parameters are being set) and during setup (preparing to process
! traces).  For efficiency, this parameter cache is not intended to be used
! during execution (trace processing).  Instead, any information needed by the
! process module should normally be retained in the process module's own data
! structure for use during execution.
!
! During trace processing, it is an error to get process parameters, global
! parameters, control parameters, gui parameters (except for messages), or
! IPN because these values are different for each process module in the job
! and therefore are likely to be incorrect during trace processing since the
! contents of the parameter cache do not change (except for possible
! accumulation of messages).  Therefore, these parameters are cleared
! (including all messages), and IPN is set to zero, by the parameter cache
! before trace processing starts.
!
! During trace processing, it is also an error to set any parameters or state
! variables (except for messages).
!
! It is also an error at any time to set any project data or job data
! parameters unless the IPN is 1 or 2 respectively.
!
! During trace processing, only the following routines are likely to be
! useful when called from a process module:
!        PC_PRINT    PC_INFO       PC_GET_PDATA   PC_ALLOC_PDATA
!        PC_ERROR    PC_WARNING    PC_GET_JDATA   PC_ALLOC_JDATA
!        PC_GET_LUN
! But please note the following points:
! (a) The message-reporting routines are likely to do nothing useful
!      except print the message.
! (b) Calling PC_ERROR is not a substitute for setting NTR = FATAL_ERROR
!      during trace processing.  During trace processing, errors must
!      be reported by setting NTR = FATAL_ERROR.  It is recommended that
!      PC_ERROR also be called.
! (c) For efficincy's sake, it is usually better to get from the
!      parameter cache what you need at update time and store it in
!      your own data structure for use at trace processing time instead
!      of calling the above routines during trace processing.
! (d) Efficiency is not an issue at update or wrapup or delete time.
!
! To enforce illegal efforts to get or set inappropriate values in the
! parameter cache at illegal times, the parameter cache purposely aborts
! with a message if such attempts are made.
!
! Calls to PC_ERROR, PC_WARNING, and PC_INFO during trace processing will
! allow messages to accumulate in the parameter cache.  These messages will
! normally be printed when received, but this allows for the eventuality
! of an interactive processing system popping up the messages in a dialog box
! during processing as they appear, or after processing has terminated normally
! or abnormally.
!
!-------------------------------------------------------------------------------
!                         PARAMETERS AND KEYWORDS
!
! The parameters stored in the parameter cache can be scalar data values or
! arrays of data values.  These data values can be integer, real, double
! precision, logical, or character variables, or variables of type(grid_struct).
! The parameters stored in the parameter cache are associated with only a
! single process module at any one time.
!
! Since the parameter values are stored in the parameter cache as character
! strings, the type does not have to match from one subroutine call to another.
! Logical values are stored internally as the strings 'YES' or 'NO'.
! Nil values are stored internally as a blank character string.
! Nil values are defined in the NAMED_CONSTANTS module.
!
! The parameters are organized into several classes which are listed in the
! following table.  Each parameter is identified by a KEYWORD which must be
! unique within a given parameter class.  (For GUI parameters, the combination
! of KEYWORD and ACTION must be unique.)  Keywords are case-insensitive.
! Any process or calling program can create any parameter in any class simply
! by calling an appropriate subroutine with a unique keyword.
!
! Keywords can be specified in either lower or upper or mixed case.
! Keywords are always saved in upper case when saved in the parameter cache.
! Keyword matching is always done in a case-insensitive manner.
! Keywords output from the parameter cache are always output in upper case.
!
! Except for GUI parameters, this parameter cache is completely ignorant of
! the meanings of the parameters it contains.
!
!
!      class of parameter                     description
!    -----------------------  --------------------------------------------
!    project data parameters  Parameters from the Project Data screen.
!                             These are OUTPUT only by the project data process.
!                             These are only INPUT by other processes.
!
!    job data parameters      Parameters from the Job Data screen.
!                             These are OUTPUT only by the job data process.
!                             These are only INPUT by other processes.
!
!    global parameters        Parameters passed from process to process and
!                               (usually) optionally changed by a process.
!                             These are initially OUTPUT only by the project
!                               data and job data processes.
!                             These are INPUT and OUTPUT by other processes.
!
!    process parameters       Parameters unique to a single process module.
!                             These are both INPUT and OUTPUT by a process.
!
!    control parameters       Parameters OUTPUT by a process to provide
!                               information to the calling program.
!
!    GUI parameters           Special parameters passed between a process and
!                               a GUI.  Some are OUTPUT by a process and others
!                               are INPUT to the process.  Some of these
!                               parameters are associated with process
!                               parameters and have the same keywords.  Others
!                               are messages to be printed or passed to a GUI.
!                             This class differs from the other classes by
!                               having one or more ACTIONs associated with
!                               each keyword, and by the fact that the
!                               parameter cache understands much of the
!                               contents of this class.  Special convenience
!                               routines are supplied for accessing the
!                               parameters in this class.  Data cards are
!                               not normally, but can be, accessed for this
!                               class.
!
!-------------------------------------------------------------------------------
!                            STATE VARIABLES
!
! In addition to the classes of parameters which are maintained by the
! parameter cache, there are also several state variables which are maintained
! by the parameter cache.  They are not associated with any keywords or
! parameters or data cards.
!
!
!       state variable                     description
!   ---------------------   ------------------------------------------------
!   integer  update_state   One of the named constants listed here.
!                           PC_GUI means this is a GUI update.
!                           PC_QUICK means this is a quick frontend update.
!                           PC_FRONTEND means this is a frontend update.
!                           PC_BACKEND means this is a backend update.
!                           PC_EXECUTE means that trace processing has begun
!                             after a backend update was performed.
!                           PC_BACKEND_NO_EXEC means this is a backend update
!                             but this process is not to process traces.
!                           See below for detailed description of update states.
!
!   integer  lun            Logical unit number for printing.
!                           This is always a valid unit number, but sometimes
!                             it will point to a log file, or to /dev/null
!                             which will discard the output.
!                           Processes should normally use this unit number
!                             rather than using PRINT or WRITE(6) to print to
!                             standard out.
!
!   integer  ipn            Process number (index of the process in the job).
!                           For processing streams in the Conoco Processing
!                             System, the project data module should have
!                             IPN=1 and the job data module should have IPN=2.
!                           The process number is NOT to be reset for processes
!                             called internally from another process.  Such
!                             an internally-called process will therefore
!                             know the process number of its calling process,
!                             and will recognize itself as internally called
!                             because the process associated with this IPN
!                             will be a different process.
!
!   logical  prev_error     True if any error has occurred in the update
!                             subroutine of any process prior to (and
!                             including) this process in the process flow.
!                           This capability allows all setups to be tested
!                             even if an early setup fails.
!
!-------------------------------------------------------------------------------
!                  DETAILED DESCRIPTION OF UPDATE STATES
!
! GUI updates (update_state = PC_GUI):
!
!   GUI updates occur whenever a single scalar parameter or a single array
!   element is modified interactively in a GUI, or when some other GUI action
!   occurs such as pressing a button or leaving a screen.  This can happen
!   repeatedly in the frontend of a processing system, or from any other
!   program which does multiple updates.  A sequence of GUI updates must
!   always be eventually followed by a frontend (or possibly a backend) update.
!
! Frontend updates (update_state = PC_FRONTEND):
!
!   Frontend updates are all non-GUI updates in the interactive frontend of
!   a processing system, or in any other program which does multiple updates
!   and does not immediately proceed to process traces.  Frontend updates
!   may occur with or without any changes to one or more (or all) parameters.
!   For example, a frontend update might occur whenever a workfile is created
!   or updated, or when a process module is inserted into the job flow, or
!   when a dialog box for a process module is popped down.
!
! Quick updates (update_state = PC_QUICK):
!
!   Quick updates are similar to frontend updates except that they are faster
!   because no traps are called.  Quick updates should be used ONLY if no
!   process parameters or GUI parameters are placed into the parameter cache.
!   Quick updates must always be eventually followed by a frontend (or possibly
!   a backend) update.
!
! Backend updates (update_state = PC_BACKEND):
!
!   A backend update is nearly identical to a frontend update.  A backend
!   update must be the last (or only) update which occurs just before trace
!   processing starts.  Therefore a single backend update is normally the
!   only update in a batch processing job or in a program outside of a
!   processing system, but may follow several GUI and/or quick and/or
!   frontend updates in an interactive processing system or any other
!   interactive program.  
!
! Execution state (update_state = PC_EXECUTE):
!
!   This state exists only during trace processing.  It is not really an
!   update state but rather a flag indicating that trace processing is in
!   progress.  This state can exist only following a PC_BACKEND state when
!   no update errors were detected.  If there are two or more independent
!   processing loops in a job, a successful PC_EXECUTE state can be followed
!   by another PC_BACKEND state for the processes in the second loop.  During
!   the PC_EXECUTE state, most parameter cache routines cannot be called.
!
!
! These traps are called from each update state:             PC_BACKEND_NO_EXEC
!                                                              PC_BACKEND
!   type of trap             PC_GUI                PC_QUICK    PC_FRONTEND
!   ------------    ----------------------------   --------    -----------
!   scalar          if modified                    never         always    
!   array element   if inserted,removed,modified   never         never    
!   pushbutton      if pressed                     never         never   
!   array           if leaving array               never         always 
!   arrayset        if leaving arrayset            never         always
!   screen          if leaving screen              never         always
!   end             never                          never         always
!
!
! The function PC_DO_NOT_PROCESS_TRACES returns the following:
!
!   returned value                             update states
!   --------------               ----------------------------------------------
!   TRUE  (always)               PC_GUI PC_QUICK PC_FRONTEND PC_BACKEND_NO_EXEC
!   TRUE  (if there are errors)            PC_BACKEND  PC_EXECUTE
!   FALSE (if there are no errors)         PC_BACKEND  PC_EXECUTE
!   TRUE  (if pc is not initialized)
!
!
! The following is proposed but not yet implemented:
! The following is proposed but not yet implemented:
! The following is proposed but not yet implemented:
!
! The behavior of some routines differs for different update states:
!
!                                                           PC_BACKEND_NO_EXEC
!                                                           PC_BACKEND
!   routines           PC_GUI           PC_QUICK            PC_FRONTEND
!   ------------       -------------    ----------          ------------------
!   PC_GET             get from the     do                  get from the    
!   PC_ALLOC           GUI parameter    nothing             process parameter
!   (without suffix)   class                                class       
!
!   PC_PUT             put into the     put into the        put into the    
!   (without suffix)   GUI parameter    process parameter   process parameter
!                      class            class               class      
!
!   PC_PUT_CONTROL     do               do                  normal
!                      nothing          nothing             operation
!
!   all GUI puts       normal           do                  do
!   except messages    operation        nothing             nothing
!
! 
!-------------------------------------------------------------------------------
!                      GUI PARAMETERS AND ACTIONS
!
! GUI parameters have actions as well as keywords associated with them.
! The keywords are process parameter keywords (scalar or array parameters),
! or the name of an arrayset (a set of linked arrays), or the name of a screen,
! or the keywords associated with messages.  The actions can be any character
! strings, some of which are understood by the parameter cache.  The actions
! are case-insensitive like keywords.  Most of them are shown here with mixed
! upper and lower case for readability.
!
! Actions can be specified in either lower or upper or mixed case.
! Actions are always saved in upper case when saved in the parameter cache.
! Action matching is always done in upper case.
! Actions output from the parameter cache are always output in upper case.
!
! Here is the list of keywords (or types of keywords) and actions used by
! the parameter cache:
!
!  keyword       action               parameter
!  --------      --------------       -------------------------------
!  array      I  'InsertIndex'        index of inserted array element.
!  array      I  'RemoveIndex'        index of removed array element.
!  array      I  'ModifyIndex'        index of modified array element.
!  scalar     B  'ModifyField'        scalar value modified.
!  array      B  'ReplaceElements'    entire array replaced.
!  scalar     O  'JumpField'          irrelevant.
!  array      O  'JumpArray'          irrelevant.
!  array      O  'JumpArrayElement'   array index to jump to.
!  arrayset   O  'JumpArrayset'       irrelevant.
!  arrayset   O  'JumpArraysetRow'    array index to jump to.
!  screen     O  'JumpScreen'         irrelevant.
!  scalar     O  'SensitiveField'     sensitivity (true/false).
!  array      O  'SensitiveArray'     sensitivity (true/false).
!  arrayset   O  'SensitiveArrayset'  sensitivity (true/false).
!  screen     O  'SensitiveScreen'    sensitivity (true/false).
!  (any)      O  'Visible'            visibility  (true/false).
!  array      O  'MinsizeArray'       minimum allowed size of array.
!  arrayset   O  'MinsizeArrayset'    minimum allowed size of arrays.
!  array      O  'MaxsizeArray'       maximum allowed size of array.
!  arrayset   O  'MaxsizeArrayset'    maximum allowed size of arrays.
!  arrayset   O  'ArrayNames'       list of keywords for all arrays in arrayset.
!  scalar     O  'OptionsField'       list of allowed options.
!  array      O  'OptionsArray'       list of allowed options.
!  'ERROR'    O  'ERROR'              list of error messages.
!  'WARNING'  O  'WARNING'            list of warning messages.
!  'INFO'     O  'INFO'               list of informational messages.
!  array      I  'LeaveArray'         irrelevant.
!  arrayset   I  'LeaveArrayset'      irrelevant.
!  screen     I  'LeaveScreen'        irrelevant.
!  scalar     I  'ButtonPress'        irrelevant.
!
! The actions marked with the letter I (input) refer to information coming
! from a GUI.  This information is used by the parameter cache in some of
! the PC_GET and and PC_CALL and PC_PRESSED and PC_VERIFY and PC_ACTIVATED
! routines to decide how to modify process array parameters, whether to call
! traps, and which GUI parameter was activated.
!
! The actions marked with the letter O (output) refer to information provided
! by a process module to a GUI.  Convenience routines are provided to set these
! parameters.  These convenience routines are documented elsewhere in the
! sections entitled MESSAGES and MISCELLANEOUS GUI DIRECTIVES.
!
! The actions marked with the letter B (both input and output) refer to
! information both coming from a GUI and provided by a process module to a
! GUI.  This information is used by the parameter cache in some of the
! PC_GET routines to set scalar and array values, and in some of the PC_PUT
! routines to put scalar and array values.  A process module can also call
! the convenience routines PC_PUT_GUI_ONLY to set these actions to keep such
! parameters from going to a workfile or history file.
!
! Actions never have to be explicitly used by process modules.  Convenience
! routines (as mentioned above) are provided for process modules to set or
! retrieve those GUI parameters which they use.
!
! The front end of a processing system should use the standard GUI access
! routines documented below to set or retrieve any of these GUI parameters
! by explicitly using both the keyword and the action.
!
!-------------------------------------------------------------------------------
!</descript_doc>



!<calling_doc>
!-------------------------------------------------------------------------------
!                       INPUT AND OUTPUT ARGUMENTS
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
!                  CLEAR AND RESTORE THE PARAMETER CACHE
!
! NOTE: PC_FRONTEND_UPDATE or PC_BACKEND_UPDATE or PC_GUI_UPDATE or
!       PC_QUICK_UPDATE must be called before the parameter cache
!       is ever used!
!
!                                                 opt
!                                                  i 
!                        call pc_gui_update      (lun)
!                        call pc_quick_update    (lun)
!                        call pc_frontend_update (lun)
!                        call pc_backend_update  (lun)
!                        call pc_clear
!                        call pc_restore
!                        call pc_next
!                        call pc_backend_execute
!                        call pc_continue_backend_update
!                        call pc_set_backend_no_exec
!                        call pc_set_backend_yes_exec
!
!                        exists = pc_exists()
!
!                     +++++++++++++++++++++++++++
!
! integer lun = logical unit number for printing, or 0 or missing to
!                    inhibit most printing (except warning and error 
!                    messages), or negative to suppress all printing
!                    (even error messages).  However, any action which
!                    causes the parameter cache to stop the program
!                    will always print a message.  The value returned
!                    by pc_get_lun() will return a pointer to /dev/null
!                    if this number is 0 or missing, or the absolute
!                    value of this number otherwise.  Therefore the
!                    calling program will be able to print if this
!                    number is negative, even though the parameter
!                    cache will not print in that case.
!
! logical exists = true if the parameter cache is active.
!
! The parameter cache is active if there is a current instance of the
! parameter cache in use.  An instance of the parameter cache is created
! whenever PC_FRONTEND_UPDATE or PC_BACKEND_UPDATE or PC_GUI_UPDATE or
! PC_QUICK_UPDATE is called, and is deleted when PC_RESTORE is called.
!
!                     +++++++++++++++++++++++++++
!
! Summary of the operation of these subroutines:
!
!                          creates
!                            new     sets
!                          instance  update
! subroutines               of PC    state      sets IPN    classes cleared
! -----------------         -----  ----------   ----------  ---------------
! PC_GUI_UPDATE              yes   PC_GUI       set to 0    all classes
! PC_QUICK_UPDATE            yes   PC_QUICK     set to 0    all classes
! PC_FRONTEND_UPDATE         yes   PC_FRONTEND  set to 0    all classes
! PC_BACKEND_UPDATE          yes   PC_BACKEND   set to 0    all classes
! PC_CLEAR                   yes       no          no       process,control,GUI
! PC_RESTORE                reverts  reverts     reverts    reverts to previous
! PC_NEXT                     no       no      incremented  process,control,GUI
! PC_BACKEND_EXECUTE          no   PC_EXECUTE        no     process,control,GUI
! PC_CONTINUE_BACKEND_UPDATE  no   PC_BACKEND        no     none
! PC_SET_BACKEND_NO_EXEC      no PC_BACKEND_NO_EXEC  no     none
! PC_SET_BACKEND_YES_EXEC     no   PC_BACKEND        no     none
!
!                     +++++++++++++++++++++++++++
!
! PC_FRONTEND_UPDATE, PC_BACKEND_UPDATE, PC_GUI_UPDATE, and PC_QUICK_UPDATE:
!   (1) Save all parameters and state variables onto a stack.
!   (2) Delete all parameters.
!   (3) Initialize state variables to these values:
!          UPDATE_STATE = PC_FRONTEND or PC_BACKEND or PC_GUI or PC_QUICK
!          PREV_ERROR   = .false.
!          LUN          = value of argument (or /dev/null if zero or missing)
!          IPN          = 0
!   (4) IPN must then be reset appropriately by calling PC_SET_IPN if
!        updating only a single process module.
!         
! PC_CLEAR:
!   (1) Saves all parameters and state variables onto a stack.
!   (2) Retains all project data, job data, and global parameters.
!   (3) Deletes all process, control, and gui parameters.
!   (4) Retains state variables.
!   (5) Sets a flag to retain new messages when PC_RESTORE is called.
!         
! PC_RESTORE:
!   (1) Restores all parameters and state variables from the stack.
!   (2) Retains all new messages reported since PC_CLEAR was last called,
!        if PC_RESTORE was preceded by PC_CLEAR.
!
! PC_NEXT:
!   (1) Does NOT save anything onto a stack.
!   (2) Retains all project data, job data, and global parameters.
!   (3) Deletes all process, control, and gui parameters.
!   (4) Retains state variables and increments IPN.
!         
! PC_BACKEND_EXECUTE:
!   (1) Does NOT save anything onto a stack.
!   (2) Retains all project data and job data parameters.
!   (3) Deletes all process, control, and gui parameters.
!   (3) Retains global parameters in case PC_CONTINUE_BACKEND_UPDATE is called.
!   (4) Retains IPN in case PC_CONTINUE_BACKEND_UPDATE is called.
!   (5) Retains state variables.
!   (6) Sets UPDATE_STATE to PC_EXECUTE.
!   (7) Aborts with an error message if PC_DO_NOT_PROCESS_TRACES returns true.
!         
! PC_CONTINUE_BACKEND_UPDATE:
!   (1) Resets UPDATE_STATE back to PC_BACKEND.
!   (2) Aborts with an error message if PC_DO_NOT_PROCESS_TRACES returns true.
!         
! PC_BACKEND_NO_EXEC:
!   (1) Sets UPDATE_STATE to PC_BACKEND_NO_EXEC.
!   (2) Aborts with an error message if the update state is not PC_BACKEND.
!         
! PC_BACKEND_YES_EXEC:
!   (1) Resets UPDATE_STATE to PC_BACKEND.
!   (2) Aborts with an error message if the update state is not PC_BACKEND.
!         
!                     +++++++++++++++++++++++++++
!
! PC_FRONTEND_UPDATE (or PC_QUICK_UPDATE) and PC_RESTORE should always be
! called in pairs:
!
!    These calls should bracket calls to all the processes in a job flow
!    in a front-end processing job, or one or more processes called from
!    outside the processing system, when this sequence is NOT TO BE FOLLOWED
!    by trace processing.
!
!    The idea is that the previous contents of the parameter cache are
!    irrelevant to the current requirements but are to be completely
!    restored afterwards.
!
!    The code sequence should be as follows:
!      - Call PC_FRONTEND_UPDATE (or PC_QUICK_UPDATE).
!      - Consecutively for each process in the job flow:
!          - Call PC_NEXT.
!          - Optionally put process parameters into the parameter cache
!             (but NEVER for PC_QUICK_UPDATE).
!          - Create, initialize, or update the process.
!          - Optionally get parameters from the parameter cache.
!      - Call PC_RESTORE.
!
!    When updating only a single process (outside of a processing system,
!    or from the front end of a processing system), the call to PC_NEXT is
!    not needed if that process can live with IPN = 0.  Alternatively (and
!    preferably), PC_SET_IPN should be called to set the proper value of IPN.
!
!                     +++++++++++++++++++++++++++
!
! PC_BACKEND_UPDATE and PC_RESTORE should always be called in pairs:
!
!    These calls should bracket calls to all the processes in a job flow
!    in a back-end processing job, or one or more processes called from
!    outside the processing system, when this sequence WILL BE FOLLOWED
!    by trace processing.
!
!    The idea is that the previous contents of the parameter cache are
!    irrelevant to the current requirements but are to be completely
!    restored afterwards.
!
!    The code sequence should be as follows (similar to PC_FRONTEND_UPDATE):
!      - Call PC_BACKEND_UPDATE.
!      - Consecutively for each process in the job flow:
!          - Call PC_NEXT.
!          - Optionally put process parameters into the parameter cache.
!          - Create, initialize, or update the process.
!          - Optionally get parameters from the parameter cache.
!      - Call PC_BACKEND_EXECUTE if continuing on to process traces.
!      - Do trace processing (if PC_BACKEND_EXECUTE was called).
!      - Call PC_RESTORE.
!
!    If there are two or more separate loops in the processing job, such that
!    the setups for subsequent loops are posponed until the previous loop is
!    finished, then the above code sequence should be modified as indicated
!    here:
!
!      - Call PC_BACKEND_UPDATE.
!      - Consecutively for each process in the first loop in the job flow:
!          - Call PC_NEXT.
!          - Optionally put process parameters into the parameter cache.
!          - Create, initialize, or update the process.
!          - Optionally get parameters from the parameter cache.
!      - Call PC_BACKEND_EXECUTE if continuing on to process traces.
!      - Do first loop trace processing (if PC_BACKEND_EXECUTE was called).
!   +  - Call PC_CONTINUE_BACKEND_UPDATE.
!   +  - Consecutively for each process in the second loop in the job flow:
!   +      - Call PC_NEXT.
!   +      - Optionally put process parameters into the parameter cache.
!   +      - Create, initialize, or update the process.
!   +      - Optionally get parameters from the parameter cache.
!   +  - Call PC_BACKEND_EXECUTE if continuing on to process traces.
!   +  - Do second loop trace processing (if PC_BACKEND_EXECUTE was called).
!      - Repeat the lines marked (+) for third or subsequent loops.
!      - Call PC_RESTORE.
!
!                     +++++++++++++++++++++++++++
!
! PC_GUI_UPDATE and PC_RESTORE should always be called in pairs:
!
!    These calls should bracket a single process being updated from a GUI
!    in a front-end processing system or an interactive processing system,
!    or any interactive program using a GUI to choose process parameters.
!
!    The idea is that the previous contents of the parameter cache are
!    irrelevant to the current requirements but are to be completely
!    restored afterwards.
!
!    The code sequence should be as follows:
!      - Call PC_GUI_UPDATE.
!      - Put the appropriate project data, job data, and global parameters
!          for this process into the parameter cache.  This is necessary
!          because more than one process can be in the condition of receiving
!          updates simultaneously, and the input globals for different
!          processes are not necessarily the same.
!      - Put process and GUI parameters from the GUI into the parameter cache.
!      - Set IPN correctly for this process.
!      - Update the process.
!      - Get process and GUI parameters from the parameter cache for the GUI.
!      - Call PC_RESTORE.
!
!    A collection of repeated GUI updates to one or more processes in the
!    process flow should be preceded and followed by one of the following
!    combinations:
!           PC_QUICK_UPDATE and PC_RESTORE
!           PC_FRONTEND_UPDATE and PC_RESTORE
!           PC_BACKEND_UPDATE and PC_RESTORE
!    The PC_QUICK_UPDATE combination should always be eventually followed by 
!    either the PC_FRONTEND_UPDATE or the PC_BACKEND_UPDATE combination.
!    The PC_FRONTEND combination would normally be used in the front end of
!    a batch processing system.  The PC_BACKEND combination (possibly preceded
!    by PC_FRONTEND combinations) should be used in an interactive processing
!    system (or any program which will process traces) just before trace
!    processing is to begin.
!
!                     +++++++++++++++++++++++++++
!
! PC_CLEAR and PC_RESTORE should always be called in pairs:
!
!   These calls should bracket a single process called internally from
!   another process.
!
!   The idea is that some of the previous contents of the parameter cache
!   (project data, job data, globals, and state variables) are to be passed
!   to the process, but any changes made by the process (except for new
!   messages) are NOT to be retained in the parameter cache.
!
!    The code sequence should be as follows:
!      - Call PC_CLEAR.
!      - Put any required changes or additions to process or global
!          parameters into the parameter cache.
!      - Create or update the process.
!      - Optionally get parameters from the parameter cache.
!      - Call PC_RESTORE.
!
!                     +++++++++++++++++++++++++++
!
! PC_NEXT should NOT be followed by PC_RESTORE:
!
!   This call should precede calling each process in a back-end processing
!   job, or each process in a front-end processing job except for GUI updates,
!   or each process in a group of processes used together.
!
!   The idea is that any changes made to the project data, job data, or
!   globals by a process are to be passed to the next process, and the
!   IPN should be incremented.
!
!    The code sequence should be as follows:
!      - Call PC_NEXT.
!      - Optionally put process parameters into the parameter cache.
!      - Create, initialize, or update the process.
!      - Optionally get parameters from the parameter cache.
!
!                     +++++++++++++++++++++++++++
!
! PC_BACKEND_NO_EXEC and PC_BACKEND_YES_EXEC should always be called in pairs:
!
!    These calls should bracket calls to create or update a process in a
!    back-end processing job if that particular process is not to process
!    traces.  The purpose may be to turn off processes in a CPU-dependent
!    manner in multiple-cpu parallel processing jobs.
!
!-------------------------------------------------------------------------------
!           TO GET AND SET STATE VARIABLES AND GET ERROR CONDITIONS
!
!                  o
!                update_state = pc_get_update_state      ()
!                lun          = pc_get_lun               ()
!                ipn          = pc_get_ipn               ()
!                update_error = pc_update_error          ()
!                prev_error   = pc_previous_error        ()
!                stop_now     = pc_do_not_process_traces ()
!
!                                            i
!                          call pc_set_ipn (ipn)
!
!                     +++++++++++++++++++++++++++
!
! integer  update_state = one of the named constants PC_FRONTEND, PC_QUICK,
!                           PC_GUI, PC_BACKEND, PC_BACKEND_NO_EXEC, or
!                           PC_EXECUTE.
! integer  lun          = logical unit number for printing (always valid).
! integer  ipn          = process number (index of process in the job).
! logical  update_error = whether any errors were reported for this process.
! logical  prev_error   = whether any errors were reported for any process.
! logical  stop_now     = whether to stop before processing traces.
!
!                     +++++++++++++++++++++++++++
!
! The returned LUN will point to /dev/null which discards the output if the
! LUN argument to PC_FRONTEND_UPDATE, PC_GUI_UPDATE, PC_BACKEND_UPDATE, or
! PC_QUICK_UPDATE was missing or 0.  Otherwise the returned LUN will be the
! absolute value of the aforementioned LUN argument.
!
! The subroutine PC_SET_IPN should be called by the front-end processing
! system only, and then only after calling PC_GUI_UPDATE (or after calling
! PC_FRONTEND_UPDATE or PC_QUICK_UPDATE if only a single process module is
! being updated).
!
! The function PC_UPDATE_ERROR() will return true if any error messages have
! been reported since PC_FRONTEND_UPDATE or PC_BACKEND_UPDATE or PC_GUI_UPDATE
! or PC_QUICK_UPDATE or PC_CLEAR or PC_NEXT was last called.  Errors are
! reported by calls to PC_ERROR, and by a PC_GET or PC_ALLOC routine if an
! error occurs there.
!
! The function PC_PREVIOUS_ERROR() will return true if any error messages have
! been reported since PC_FRONTEND_UPDATE or PC_BACKEND_UPDATE or PC_GUI_UPDATE
! or PC_QUICK_UPDATE was last called.  This normally includes this process
! and possibly all preceding processes in the job.
!
! The function PC_DO_NOT_PROCESS_TRACES() is a convenience routine which
! will return true for any one of these conditions:
!        True if pc_get_update_state() is not PC_BACKEND or PC_EXECUTE.
!        True if pc_update_error() is true.
!        True if pc_previous_error() is true.
!        True if the parameter cache is not initialized.
!        False otherwise.
!
!-------------------------------------------------------------------------------
!           TO TRANSFER DATA CARDS TO AND FROM THE PARAMETER CACHE
!
!           o
!         ncards = pc_num_process_cards ()
!         ncards = pc_num_global_cards  ()
!         ncards = pc_num_control_cards ()
!         ncards = pc_num_pdata_cards   ()
!         ncards = pc_num_jdata_cards   ()
!         ncards = pc_num_gui_cards     ()
!
!                                          o       o
!         call pc_alloc_process_cards   (pcards, ncards)
!         call pc_alloc_global_cards    (pcards, ncards)
!         call pc_alloc_control_cards   (pcards, ncards)
!         call pc_alloc_pdata_cards     (pcards, ncards)
!         call pc_alloc_jdata_cards     (pcards, ncards)
!         call pc_alloc_gui_cards       (pcards, ncards)
!
!                                          o      o       o
!         call pc_get_process_cards     (cards, ncards, errmsg)
!         call pc_get_global_cards      (cards, ncards, errmsg)
!         call pc_get_control_cards     (cards, ncards, errmsg)
!         call pc_get_pdata_cards       (cards, ncards, errmsg)
!         call pc_get_jdata_cards       (cards, ncards, errmsg)
!         call pc_get_gui_cards         (cards, ncards, errmsg)
!
!                                          i     o      o
!         call pc_get_process_card      (icard, card, errmsg)
!         call pc_get_global_card       (icard, card, errmsg)
!         call pc_get_control_card      (icard, card, errmsg)
!         call pc_get_pdata_card        (icard, card, errmsg)
!         call pc_get_jdata_card        (icard, card, errmsg)
!         call pc_get_gui_card          (icard, card, errmsg)
!
!                                          i     i
!         call pc_put_process_cards     (cards,ncards)
!         call pc_put_global_cards      (cards,ncards)
!         call pc_put_control_cards     (cards,ncards)
!         call pc_put_pdata_cards       (cards,ncards)
!         call pc_put_jdata_cards       (cards,ncards)
!         call pc_put_gui_cards         (cards,ncards)
!
!                                         i 
!         call pc_put_process_card      (card)
!         call pc_put_global_card       (card)
!         call pc_put_control_card      (card)
!         call pc_put_pdata_card        (card)
!         call pc_put_jdata_card        (card)
!         call pc_put_gui_card          (card)
!
!                                         i 
!         call pc_add_process_card      (card)
!         call pc_add_global_card       (card)
!         call pc_add_control_card      (card)
!         call pc_add_pdata_card        (card)
!         call pc_add_jdata_card        (card)
!         call pc_add_gui_card          (card)
!
!
!         call pc_clear_process_cards   
!         call pc_clear_global_cards   
!         call pc_clear_control_cards 
!         call pc_clear_pdata_cards  
!         call pc_clear_jdata_cards 
!         call pc_clear_gui_cards  
!
!                     +++++++++++++++++++++++++++
!
! integer                      ncards = number of data cards.
! integer                       icard = index of desired data card (1-ncards).
! character(len=*)               card = a single data card.
! character(len=*)           cards(:) = an array of data cards.
! character(len=*),pointer  pcards(:) = pointer to an array of data cards.
! character(len=*)             errmsg = error message (blank if no error).
!
! The recommended length of data card character variables is 
! PC_DATACARD_LENGTH characters.
!
! Upon input, data cards can have any length, but data cards with information
! exceeding PC_LENGTH characters will be truncated.
!
! Upon output, the information on a single data card will not exceed
! PC_DATACARD_LENGTH characters minus at least PC_DATACARD_PADDING
! characters, unless data cards are input and output without ever being
! converted to keyword/value parameters and back, in which case the output
! data cards will be the same length as input.
!
!        | PC_LENGTH is a named constant with the same value as |
!        | STRINGLIST_LENGTH (currently 160 as of 2000-01-28)   |
!        | in the STRINGLIST primitive.                         |
!
!    | PC_DATACARD_LENGTH and PC_DATACARD_PADDING are named         |
!    | constants with the same values as CARDSET_DATACARD_LENGTH    |
!    | (currently 80 as of 2000-01-28) and CARDSET_DATACARD_PADDING |
!    | (currently 8 as of 2000-01-28) in the CARDSET primitive.     |
!
!                     +++++++++++++++++++++++++++
!
! The PC_NUM_CARDS routines:
!   (1) return the number of data cards in the specified parameter class.
!
! The PC_ALLOC_CARDS routines:
!   (1) get all of the data cards in the specified parameter class.
!   (2) PCARDS is deallocated and reallocated to contain all data cards.
!          (PCARDS must be nullified or allocated before first use.)
!          (PCARDS should be conditionally deallocated after last use.)
!   (3) PCARDS is always reallocated to at least one array element, even if
!        there are no data cards and NCARDS is set or reset to zero.
!
! The PC_GET_CARDS routines:
!   (1) get all of the data cards in the specified parameter class.
!   (2) an error occurs if CARDS is dimensioned too small for all data cards.
!   (3) does not reset CARDS or NCARDS if an error occurs.
!
! The PC_GET_CARD routines:
!   (1) get the requested data card in the specified parameter class.
!   (2) an error occurs if ICARD is out of range.
!   (3) does not reset CARD if an error occurs.
!
! The PC_PUT_CARDS routines:
!   (1) replace the previous contents with an array with 0 or more data cards.
!
! The PC_PUT_CARD routines:
!   (1) replace the previous contents with a single data card.
!
! The PC_ADD_CARD routines:
!   (1) append one data card to the previous contents.
!
! The PC_CLEAR_CARDS routines:
!   (1) delete all of the data cards in the specified parameter class.
!
!-------------------------------------------------------------------------------
!                     TO GET KEYWORD INFORMATION            
!
!          o
!        nkeys   = pc_num_process_keywords  ()
!        nkeys   = pc_num_global_keywords   ()
!        nkeys   = pc_num_control_keywords  ()
!        nkeys   = pc_num_gui_keywords      ()
!        nkeys   = pc_num_pdata_keywords    ()
!        nkeys   = pc_num_jdata_keywords    ()
!
!          o                                   i
!        keyword = pc_get_process_keyword    (ikey)
!        keyword = pc_get_global_keyword     (ikey)
!        keyword = pc_get_control_keyword    (ikey)
!        keyword = pc_get_gui_keyword        (ikey)
!        action  = pc_get_gui_action         (ikey)
!        keyword = pc_get_pdata_keyword      (ikey)
!        keyword = pc_get_jdata_keyword      (ikey)
!
!          o                                       i       i
!        present = pc_process_keyword_present  (keyword)
!        present = pc_global_keyword_present   (keyword)
!        present = pc_control_keyword_present  (keyword)
!        present = pc_gui_action_present       (keyword, action)
!        present = pc_pdata_keyword_present    (keyword)
!        present = pc_jdata_keyword_present    (keyword)
!
!                                                 i       i
!             call pc_remove_process_keyword  (keyword)
!             call pc_remove_global_keyword   (keyword)
!             call pc_remove_control_keyword  (keyword)
!             call pc_remove_gui_action       (keyword, action)
!             call pc_remove_pdata_keyword    (keyword)
!             call pc_remove_jdata_keyword    (keyword)
!
!                     +++++++++++++++++++++++++++
!
! integer                  nkeys = number of keywords.
! integer                   ikey = index of desired keyword (1-nkeys).
! character(len=*)       keyword = keyword of the desired parameter.
! character(len=*)        action = action associated with this GUI parameter.
! logical                present = whether the keyword is present.
!
! KEYWORD is returned as a blank string if IKEY is out of range.
! ACTION  is returned as a blank string if IKEY is out of range.
!
! PC_GUI_ACTION_PRESENT returns false unless both the keyword and its action
!  are present.
! PC_REMOVE_GUI_ACTION removes only the specified action associated with
!  the keyword.
!
!-------------------------------------------------------------------------------
!                    TO ACCESS PARAMETERS BY KEYWORD       
!
! Get the number of array elements:
!
!         o                                    i       i
!      nelements = pc_num_elements_process (keyword)
!      nelements = pc_num_elements_global  (keyword)
!      nelements = pc_num_elements_control (keyword)
!      nelements = pc_num_elements_gui     (keyword, action)
!      nelements = pc_num_elements_pdata   (keyword)
!      nelements = pc_num_elements_jdata   (keyword)
!
!
! Get the nature of the parameter (scalar or array):
!
!        o                                     i       i
!      nature    = pc_nature_process       (keyword)
!      nature    = pc_nature_global        (keyword)
!      nature    = pc_nature_control       (keyword)
!      nature    = pc_nature_gui           (keyword, action)
!      nature    = pc_nature_pdata         (keyword)
!      nature    = pc_nature_jdata         (keyword)
!
!
! Get the variable type of the parameter:
!
!        o                                     i       i
!      vartype   = pc_vartype_process      (keyword)
!      vartype   = pc_vartype_global       (keyword)
!      vartype   = pc_vartype_control      (keyword)
!      vartype   = pc_vartype_gui          (keyword, action)
!      vartype   = pc_vartype_pdata        (keyword)
!      vartype   = pc_vartype_jdata        (keyword)
!
!
! Allocate and get array parameters (entire array):
!   (first routine accesses process and gui parameters)
!   (first routine must be called only by the owner of the process parameters)
!
!                                                                  opt
!                                i       i       o         o        i
!      call pc_alloc         (keyword,         parray, nelements, etrap)
!      call pc_alloc_process (keyword,         parray, nelements)
!      call pc_alloc_global  (keyword,         parray, nelements)
!      call pc_alloc_control (keyword,         parray, nelements)
!      call pc_alloc_gui     (keyword, action, parray, nelements)
!      call pc_alloc_pdata   (keyword,         parray, nelements)
!      call pc_alloc_jdata   (keyword,         parray, nelements)
!
!
! Get array parameters (entire array):
!   (first routine accesses process and gui parameters)
!   (first routine must be called only by the owner of the process parameters)
!
!                                                                 opt
!                                i       i       o        o        i
!      call pc_get           (keyword,         array, nelements, etrap)
!      call pc_get_process   (keyword,         array, nelements)
!      call pc_get_global    (keyword,         array, nelements)
!      call pc_get_control   (keyword,         array, nelements)
!      call pc_get_gui       (keyword, action, array, nelements)
!      call pc_get_pdata     (keyword,         array, nelements)
!      call pc_get_jdata     (keyword,         array, nelements)
!
!
! Get scalar parameters:
!   (first routine accesses process and gui parameters)
!   (first routine must be called only by the owner of the process parameters)
!
!                                                      opt
!                                i       i       o      i
!      call pc_get           (keyword,         scalar, trap)
!      call pc_get_process   (keyword,         scalar)
!      call pc_get_global    (keyword,         scalar)
!      call pc_get_control   (keyword,         scalar)
!      call pc_get_gui       (keyword, action, scalar)
!      call pc_get_pdata     (keyword,         scalar)
!      call pc_get_jdata     (keyword,         scalar)
!
!
! Get array parameters (a single array element):
!
!                                i       i      i       o
!      call pc_get_process   (keyword,         indx, element)
!      call pc_get_global    (keyword,         indx, element)
!      call pc_get_control   (keyword,         indx, element)
!      call pc_get_gui       (keyword, action, indx, element)
!      call pc_get_pdata     (keyword,         indx, element)
!      call pc_get_jdata     (keyword,         indx, element)
!
!
! Call pushbutton and array and screen traps:
!   (these routines must be called only by the owner of the process parameters)
!
!                                            opt
!         o                           i       i
!      pressed = pc_pressed       (keyword, ptrap)
!
!                                     i       i
!      call pc_call_array_trap    (keyword, atrap)
!      call pc_call_arrayset_trap (keyword, astrap)
!      call pc_call_screen_trap   (keyword, strap)
!      call pc_call_end_trap               (endtrap)
!
!
! Determine which GUI field was activated:
!   (must be called only by the owner of the process parameters)
!
!         o 
!      keyword = pc_activated ()
!
!
! Determine whether a parameter should be verified:
!   (these routines must be called only by the owner of the process parameters)
!
!         o                             i      o      o
!      verify  = pc_verify_scalar   (keyword)                    ! trap
!      verify  = pc_verify_element  (keyword, indx, action)      ! etrap
!      verify  = pc_verify_array    (keyword)                    ! atrap
!      verify  = pc_verify_arrayset (keyword)                    ! astrap
!      verify  = pc_verify_screen   (keyword)                    ! strap
!      verify  = pc_verify_end      ()                           ! endtrap
!
!
! Put array parameters (entire array):
!   (first routine might change both process and gui parameters)
!   (first routine must be called only by the owner of the process parameters)
!
!                                                                opt   opt
!                               i       i       i        i        i     i
!      call pc_put          (keyword,         array, nelements, nchar, ndec)
!      call pc_put_process  (keyword,         array, nelements, nchar, ndec)
!      call pc_put_global   (keyword,         array, nelements, nchar, ndec)
!      call pc_put_control  (keyword,         array, nelements, nchar, ndec)
!      call pc_put_gui      (keyword, action, array, nelements, nchar, ndec)
!      call pc_put_gui_only (keyword,         array, nelements, nchar, ndec)
!      call pc_put_pdata    (keyword,         array, nelements, nchar, ndec)
!      call pc_put_jdata    (keyword,         array, nelements, nchar, ndec)
!
!
! Put scalar parameters:
!   (first routine might change both process and gui parameters)
!   (first routine must be called only the owner of the process parameters)
!
!                                                      opt   opt
!                               i       i       i       i     i
!      call pc_put          (keyword,         scalar, nchar, ndec)
!      call pc_put_process  (keyword,         scalar, nchar, ndec)
!      call pc_put_global   (keyword,         scalar, nchar, ndec)
!      call pc_put_control  (keyword,         scalar, nchar, ndec)
!      call pc_put_gui      (keyword, action, scalar, nchar, ndec)
!      call pc_put_gui_only (keyword,         scalar, nchar, ndec)
!      call pc_put_pdata    (keyword,         scalar, nchar, ndec)
!      call pc_put_jdata    (keyword,         scalar, nchar, ndec)
!
!                     +++++++++++++++++++++++++++
!
! character(len=*)      keyword = keyword of the desired parameter.
! character(len=*)       action = action associated with this GUI parameter.
! integer             nelements = number of array elements.
! integer                nature = nature of the parameter.
! integer               vartype = variable type of the parameter.
! integer (pc_verify...)   indx = index of array element (1-nelements).
! integer (pc_verify...) action = PC_INSERT, PC_REMOVE, PC_MODIFY, PC_NOACTION.
! logical               pressed = true if the pushbutton was pressed.
! logical                verify = true if the parameter should be verified.
! (any type)             scalar = single scalar parameter value.
! (any type)            element = individual array element.
! (any type)           array(:) = array of parameter values.
! (any type),pointer  parray(:) = pointer to array of parameter values.
! external  ,optional      trap = scalar trap to call.
! external  ,optional     etrap = array element trap to call.
! external  ,optional     ptrap = pushbutton trap to call.
! external                atrap = array trap to call.
! external               astrap = arrayset trap to call.
! external                strap = screen trap to call.
! external              endtrap = end trap to call.
! integer   ,optional     nchar = maximum number of characters to encode.
! integer   ,optional      ndec = maximum number of decimals to encode.
!
! The type of ARRAY and PARRAY and SCALAR and ELEMENT can be real, integer,
! double precision, logical, or character(len=*).  The type of SCALAR can also
! be type(grid_struct).  Character variables exceeding length PC_LENGTH
! will be truncated.
!
!        | PC_LENGTH is a named constant with the same value as |
!        | STRINGLIST_LENGTH (currently 160 as of 2000-01-28)   |
!        | in the STRINGLIST primitive.                         |
!
! The NCHAR argument is for integer, real, and double precision variables only.
! The NDEC argument is for real and double precision variables only.
! The NCHAR and NDEC arguments are both used for type(grid_struct).
! No maximum restrictions are imposed if NCHAR and NDEC are not specified.
!
! When numeric values are encoded into character strings for storage in the
! parameter cache and subsequent output to data cards or to a GUI or back
! to numeric values, non-significant digits (restricted by the precision
! of the word size) are never encoded into the character string.  Therefore,
! NDEC normally need not be specified unless you really want to round the value
! to the specified number of decimals.  Also, NCHAR needs to be specified only
! if you want to restrict the size of the encoded value to the maximum number
! of characters displayed in a GUI and to display an asterisk (*) if the
! value cannot be made to fit (even with reduced precision); but be aware
! that subsequent efforts to get this parameter from the parameter cache will
! be unsuccessful in this case.
!
!                     +++++++++++++++++++++++++++
!
! PC_NUM_ELEMENTS routines:
!   (1) return number of elements (0 or more) if the parameter is an ARRAY.
!   (2) return              1                 if the parameter is a SCALAR.
!   (3) return              0                 if KEYWORD is not found.
!
! PC_NATURE routines:
!   (1) return named constant PC_ARRAY   if the parameter is an ARRAY.
!   (2) return named constant PC_SCALAR  if the parameter is a SCALAR.
!   (3) return named constant PC_MISSING if KEYWORD is not found.
!
! PC_ALLOC routines for arrays:
!   (1) get all of the array elements (0 or more) in the parameter cache.
!   (2) PARRAY is deallocated and reallocated to contain all elements.
!          (PARRAY must be nullified or allocated before first use.)
!          (PARRAY should be conditionally deallocated after last use.)
!   (3) PARRAY is always reallocated to at least one array element, even if
!        there are no array elements and NELEMENTS is set or reset to zero.
!   (4) call PC_ERROR if the parameter is a scalar.
!   (5) call PC_ERROR if any element cannot be decoded into the desired type.
!   (6) do not reset PARRAY or NELEMENTS if KEYWORD is not found.
!   (7) do not reset PARRAY or NELEMENTS if PC_ERROR was called.
!   (8) call the array element trap if the array was successfully modified
!        by inserting, removing, or changing a single array element in a GUI.
!
! PC_GET routines for arrays:
!   (1) get all of the array elements (0 or more) in the parameter cache.
!   (2) call PC_ERROR if ARRAY is dimensioned too small for all elements.
!   (3) call PC_ERROR if the parameter is a scalar.
!   (4) call PC_ERROR if any element cannot be decoded into the desired type.
!   (5) do not reset ARRAY or NELEMENTS if KEYWORD is not found.
!   (6) do not reset ARRAY or NELEMENTS if PC_ERROR was called.
!   (7) call the array element trap if the array was successfully modified
!        by inserting, removing, or changing a single array element in a GUI.
!
! PC_GET routines for scalars:
!   (1) get the requested scalar value in the parameter cache.
!   (2) call PC_ERROR if the parameter is an array.
!   (3) call PC_ERROR if the scalar cannot be decoded into the desired type.
!   (4) do not reset SCALAR if KEYWORD is not found.
!   (5) do not reset SCALAR if PC_ERROR was called.
!   (6) call the scalar trap if the scalar value is successfully modified.
!   (7) always call the scalar trap if this is not a GUI update (so that
!        the trap code need not be duplicated elsewhere).
!
! PC_GET routines for individual array elements:
!   (1) get the requested array element in the parameter cache.
!   (2) call PC_ERROR if the array index INDX is out of range.
!   (3) call PC_ERROR if the parameter is a scalar.
!   (4) call PC_ERROR if the element cannot be decoded into the desired type.
!   (5) do not reset ELEMENT if KEYWORD is not found.
!   (6) do not reset ELEMENT if PC_ERROR was called.
!
! PC_PRESSED routine for scalar pushbuttons:
!   (1) returns true if the specified pushbutton was pressed.
!   (2) returns false if the specified pushbutton was not pressed.
!   (3) calls the pushbutton trap if the pushbutton was pressed.
!
! PC_ACTIVATED routine for all GUI fields:
!   (1) returns the keyword where a value was modified or button pressed.
!   (2) returns a blank if no value was modified or button pressed.
!   (3) returns a blank if this is not a GUI update.
!   (4) returns a blank if there are more than one modified field.
!   (5) returns a blank if an array had components inserted or removed,
!        because for array sets this would be more than one keyword.
!
! PC_CALL_ARRAY_TRAP:
!   (1) calls the array trap when leaving the array field in the GUI.
!   (2) always calls the array trap if this is not a GUI update (so that
!        the trap code need not be duplicated elsewhere).
!   (3) the calls to this routine should always follow the calls to the
!         PC_GET routines.
!
! PC_CALL_ARRAYSET_TRAP:
!   (1) calls the arrayset trap when leaving the arrayset field (set of
!        linked arrays) in the GUI.
!   (2) always calls the arrayset trap if this is not a GUI update (so that
!        the trap code need not be duplicated elsewhere).
!   (3) the calls to this routine should always follow the calls to the
!         PC_CALL_ARRAY_TRAP routines.
!
! PC_CALL_SCREEN_TRAP:
!   (1) calls the screen trap when leaving the screen in the GUI.
!   (2) always calls the screen trap if this is not a GUI update (so that
!        the trap code need not be duplicated elsewhere).
!   (3) the calls to this routine should always follow the calls to the
!         PC_CALL_ARRAYSET_TRAP routines.
!
! PC_CALL_END_TRAP:
!   (1) calls the end trap if this is not a GUI update.
!   (2) the calls to this routine should always follow the calls to the
!         PC_CALL_SCREEN_TRAP routines.
!
! PC_VERIFY routines:
!   (1) return true if the specified parameter should be verified.
!   (2) return false if the specified parameter need not be verified.
!   (3) return the same logical used to call the specified traps.
!   (4) PC_VERIFY_ELEMENT returns the same INDX and ACTION passed to ETRAP,
!        or 0 and PC_NOACTION if ETRAP would not be called.
!
! PC_PUT routines for scalars:
!   (1) first create a new parameter if KEYWORD is not found.
!   (2) replace the previous contents with a scalar.
!   (3) set the nature of the parameter to be a SCALAR.
!   (4) set the variable type of the parameter.
!
! PC_PUT routines for arrays:
!   (1) first create a new parameter if KEYWORD is not found.
!   (2) replace the previous contents with an array with 0 or more elements.
!   (3) set the nature of the parameter to be an ARRAY.
!   (4) set the variable type of the parameter.
!
! PC_PUT_GUI_ONLY routines for scalars and arrays:
!   (1) same as the PC_PUT routines except that the information goes only
!         to the GUI and not to any workfile or history file.
!
!-------------------------------------------------------------------------------
!                                TRAPS
!
! The traps called by the PC_GET and PC_ALLOC and PC_CALL and PC_PRESSED
! routines must look like this:
!
!   | subroutine trap (keyword)                ! scalar trap.
!   | character(len=*),intent(in) :: keyword   ! scalar parameter keyword.
!   | .................
!   | end subroutine trap
!
!
!   | subroutine etrap (keyword,indx,action)   ! array element trap.
!   | character(len=*),intent(in) :: keyword   ! array parameter keyword.
!   | integer         ,intent(in) :: indx      ! index where change occurred.
!   | integer         ,intent(in) :: action    ! one of the named constants
!   |                                          !      PC_INSERT
!   |                                          !   or PC_REMOVE
!   |                                          !   or PC_MODIFY.
!   |  ! INDX is a Fortran-style index (=1 for the first array element).
!   |  ! INDX refers to the array element inserted or removed or modified.
!   | .................
!   | end subroutine etrap
!
!
!   | subroutine ptrap (keyword)               ! pushbutton trap.
!   | character(len=*),intent(in) :: keyword   ! pushbutton keyword.
!   | .................
!   | end subroutine ptrap
!
!
!   | subroutine atrap (keyword)               ! array trap.
!   | character(len=*),intent(in) :: keyword   ! array parameter keyword.
!   | .................
!   | end subroutine atrap
!
!
!   | subroutine astrap (keyword)              ! arrayset trap.
!   | character(len=*),intent(in) :: keyword   ! arrayset keyword.
!   | .................
!   | end subroutine astrap
!
!
!   | subroutine strap (keyword)               ! screen trap.
!   | character(len=*),intent(in) :: keyword   ! screen keyword.
!   | .................
!   | end subroutine strap
!
!
!   | subroutine endtrap                       ! end trap.
!   | .................
!   | end subroutine endtrap
!
!-------------------------------------------------------------------------------
!                         WHEN TRAPS ARE CALLED
!
! Pushbutton traps:
!   (1) Whenever the pushbutton is pressed in a GUI.
!   (2) Never called at any other time.
!
! Scalar traps:
!   (1) Whenever the scalar value is successfully modified in a GUI.
!   (2) Always called when this is not a GUI update.
!
! Array element traps:
!   (1) Whenever the array is successfully modified in a GUI
!        by inserting, removing, or changing a single array element.
!   (2) Never called at any other time.
!   (3) At a minimum, the specified array element should be verified in
!        this trap.
!
! Array traps:
!   (1) When leaving the array field in the GUI, as long as the array is
!        not part of a set of linked arrays.
!   (2) An arrayset trap should be used instead of an array trap for an
!        array which is part of a linked array set.
!   (3) Always called when this is not a GUI update.
!   (4) It is guaranteed that an array trap will always be eventually
!        called after one or more calls to the corresponding array
!        element trap.
!   (5) The entire array should be verified in this trap.
!
! Arrayset traps:
!   (1) When leaving the arrayset field (set of linked arrays) in the GUI.
!   (2) An array trap should be used instead of an arrayset trap for a
!        single array which is not part of a linked array set.
!   (3) Always called when this is not a GUI update.
!   (4) It is guaranteed that an arrayset trap will always be eventually
!        called after one or more calls to the corresponding array
!        element traps.
!   (5) The entire set of linked arrays should be verified in this trap.
!   (6) The keyword for a set of linked arrays is the keyword for the first
!        array in the set (leftmost in the GUI), with the suffix "_arrayset"
!        appended.
!
! Screen traps:
!   (1) When leaving the screen in the GUI.
!   (2) Always called when this is not a GUI update.
!   (3) Any variables on this screen, or interactions between such
!        variables, which have not been verified in the preceding traps
!        should be verified here.
!   (4) The keyword for a screen is the word "screen1" for the first
!        screen in the GUI for this process, with the digit incremented
!        by 1 for each additional screen.
!
! End traps:
!   (1) Never called when this is a GUI update.
!   (2) Always called when this is not a GUI update.
!   (3) An end trap can be used for general or time-consuming verifications
!        which cannot be performed in other traps but need to be performed
!        before process parameters are saved (e.g. by a front-end) or used
!        to process traces (e.g. on the back-end).
!
!                         +++++++++++++++++++++++
!
! GUI updates can generate a call to any trap except an end trap.
!
! Frontend and backend updates always generate a call to all types of traps
! except never to array element traps and pushbutton traps.
!
! In the original CPS system, traps (except the end trap) were only called
! when the user changed a value, or the user left an array field or a screen.
! This meant that the trap code often had to be duplicated in the end trap
! routine on the frontend.  And also of course, in the original CPS system,
! this code also had to be duplicated (again) on the backend.  The current
! method of determining when traps are called is designed to avoid all such
! duplication requirements in the new system.
!
! See above for a detailed description of update states.
!
!-------------------------------------------------------------------------------
!           GENERAL DISCUSSION OF THE PROCESS MODULE UPDATE ROUTINE
!                          AND WHEN TRAPS ARE CALLED
!
! When building a job flow in CFE, the process update subroutine is called
! repeatedly.  It is called when the process is first placed into the job flow,
! when the process parameter screen is popped up or down, when any other
! process parameter screen is popped down, when any other process is placed
! into or removed from the job flow, and possibly other times.  While the
! process parameter screen is popped up, the process update routine is called
! every time the user enters a parameter, exits an array or an arrayset,
! moves from one screen to another, presses a pushbutton, etc.  Each time
! the process update routine is called, there will be zero or more parameters
! in the parameter cache.  Normally, for GUI updates (e.g. when the user
! enters a parameter value), only the changed parameter will be in the
! parameter cache.  In other cases, all of the process parameters, or no
! parameters, may be in the parameter cache.  The code developer does not
! need to know how many parameters are in the parameter cache at any particular
! time the update routine is called.  Calls to PC_GET or PC_ALLOC will reset
! the parameter to its new value if it is present in the parameter cache,
! and will do nothing otherwise.
!
! When the process is created at the beginning of a batch job, the process
! update subroutine is called once, with all process parameters in the
! parameter cache.  The same code in the process update routine is executed
! whether the process is running on the front end (in CFE) or on the back end
! (in a batch job).  Only on the back end, when trace processing is about to
! commence, will the code after calling the PC_DO_NOT_PROCESS_TRACES() be
! executed.
!
! If the code developer adds a scalar trap argument to the PC_GET call for
! a scalar parameter, that scalar trap will be called from inside the PC_GET
! subroutine after the scalar value is changed.  If the code developer adds
! an array element trap argument to the PC_GET or PC_ALLOC call for an array
! parameter, that array element trap will be called from inside the PC_GET
! or PC_ALLOC subroutine after an array element is changed or inserted or
! removed.  If  PC_CALL_ARRAY_TRAP is called, the specified trap will be
! called when the user exits the array.  If PC_CALL_ARRAYSET_TRAP is called,
! the specified trap will be called when the user exits the arrayset.  If
! PC_CALL_SCREEN_TRAP is called, the specified trap will be called when the
! user exits the specified screen.  If  PC_CALL_END_TRAP is called, the
! specified trap will be called when the user pops down the process parameter
! screen for the process.  In all cases, the trap is called from inside the
! subroutine to which the specified trap is passed; the parameter cache does
! not attempt to remember the trap name for use at any other time.  Finally,
! in order to make sure all parameters are checked, all traps (except array
! element traps) are called whether or not the parameter is in the parameter
! cache for all non-GUI updates.  A non-GUI update is any call to the process
! update routine which does not originate with a user action in the GUI.
! Examples are when the workfile is updated or when the update routine is
! called from the back end.  The reason is to make sure all process parameters
! are checked whether or not the user ever changed the value.  This should
! eliminate having to write redundant parameter verification code in different
! places.
!
! If the code developer chooses not to use traps, then the parameter
! verification code should be placed into the parameter verification section
! of the update routine.  The code developer can choose whether to simply
! check the parameter every time the update routine is called (which could
! be inefficient), or to conditionally check the parameter only when a
! corresponding trap would otherwise have been called.
!
!-------------------------------------------------------------------------------
!                                MESSAGES
!
! These routines are designed to allow a process module to output messages.
! These messages are stored as GUI array parameters.  A GUI might wish to
! display the messages to a user or ring a bell for a fatal error.  These
! routines can be called from within or outside a trap.  Messages are added
! to the parameter cache one at a time.
!
!                                       i   
!                     call pc_error   (msg)
!                     call pc_warning (msg)
!                     call pc_info    (msg)
!                     call pc_print   (msg)
!
!                                                  opt   opt   opt
!                                       i     i     i     i     i
!                     call pc_error   (msg1, var1, msg2, var2, msg3)
!                     call pc_warning (msg1, var1, msg2, var2, msg3)
!                     call pc_info    (msg1, var1, msg2, var2, msg3)
!                     call pc_print   (msg1, var1, msg2, var2, msg3)
!
!
!                     call pc_print_process_cards 
!                     call pc_print_global_cards 
!                     call pc_print_control_cards
!                     call pc_print_pdata_cards 
!                     call pc_print_jdata_cards 
!                     call pc_print_gui_cards  
!
!                     call pc_info_process_cards
!                     call pc_info_global_cards 
!                     call pc_info_control_cards
!                     call pc_info_pdata_cards  
!                     call pc_info_jdata_cards  
!                     call pc_info_gui_cards    
!
!                     +++++++++++++++++++++++++++
!
! character(len=*)            msg = message to report.
! character(len=*) msg1,msg2,msg3 = portions of a message to report.
! (any type)            var1,var2 = values to be used in the message.
! 
! The routines with optional parameters will build a message from all of
! the existing input arguments.  The values will be converted from their
! actual type into character strings and placed into the message.
! The values can be type integer, real, double precision, logical, or character.
! The message will be constructed in the order of the arguments.
!
! The message input routines take the following actions:
!
! (1) All of them except PC_PRINT add a message to the parameter cache.
! (2) All of them will print to logical unit number LUN (unless LUN < 0).
! (4) The PC_PRINT subroutine will do nothing if LUN points to /dev/null.
! (5) The PC_ERROR subroutine will set the PREV_ERROR state variable to true.
! (6) The PC_ERROR subroutine will also print to standard out if LUN is not 6.
! (7) The PC_WARNING subroutine will also print to standard out if LUN is not 6.
! (8) The PC_ERROR subroutine will not print anything if input LUN was negative.
! (9) The PC_WARNING subroutine will not print anything if input LUN was negative.
!
! PC_ERROR, PC_WARNING, and PC_INFO do nothing if the message is blank.
!
! Except for error and warning messages which can go to standard out (LUN=6),
! any printing should occur only to logical unit number LUN, which must
! be obtained from this parameter cache by calling PC_GET_LUN.  If LUN
! points to /dev/null, calls to the parameter cache to print messages can
! still be done although they will have no effect (except for error and warning
! messages which will go to standard out if the input LUN was not negative).
!
! The PC_PRINT_..._CARDS routines (which call PC_PRINT for each data card)
! are useful for printing to standard out, which could be a log file or a
! report file.  A good use would be to print parameters of internally-called
! process modules into the report file.
!
! The PC_INFO_..._CARDS routines (which call PC_INFO for each data card)
! are useful for debugging a GUI.
! 
!-------------------------------------------------------------------------------
!                      MISCELLANEOUS GUI DIRECTIVES
!
! These convenience routines are designed to allow a process module to
! put various GUI-related values and directives into the parameter cache.
! These values are stored as GUI parameters by keyword and action.  The
! first routine (pc_register_array_names) should be called before calling
! the pc_get... routines.  The second and third routines (pc_put_options...)
! should be called before calling any other pc_put routines.  The other
! routines listed here can be called from within or outside a trap.
!
!                                                      opt 
!                                    i       i          i  
!   call pc_register_array_names (keyword, arrays,   narrays)
!
!                                                     opt      opt   opt
!                                    i        i        i        i     i
!   call pc_put_options_field    (keyword, options, noptions, nchar, ndec)
!   call pc_put_options_array    (keyword, options, noptions, nchar, ndec)
!
!                                            i        i
!   call pc_put_sensitive_field_flag     (keyword, sensitive)
!   call pc_put_sensitive_array_flag     (keyword, sensitive)
!   call pc_put_sensitive_arrayset_flag  (keyword, sensitive)
!   call pc_put_sensitive_screen_flag    (keyword, sensitive)
!
!                                            i        i
!   call pc_put_visible_flag             (keyword, visible)
!
!                                     i        i
!   call pc_put_minsize_array     (keyword, minsize)
!   call pc_put_minsize_arrayset  (keyword, minsize)
!   call pc_put_maxsize_array     (keyword, maxsize)
!   call pc_put_maxsize_arrayset  (keyword, maxsize)
!
!                                    i       i
!   call pc_jump_screen          (keyword)        ! specified screen.
!   call pc_jump_field           (keyword)        ! specified scalar field.
!   call pc_jump_arrayset        (keyword)        ! specified linked array set.
!   call pc_jump_arrayset_row    (keyword, indx)  ! specified row.
!   call pc_jump_array_element   (keyword, indx)  ! specified array element.
!   call pc_jump_array           (keyword)        ! specified array.
!
!                     +++++++++++++++++++++++++++
!
! character(len=*)  keyword    = keyword of the associated parameter.
! character(len=*)   arrays(:) = list of keywords of arrays in the arrayset.
! integer           narrays    = total number of arrays in the arrayset.
! (any type)        options(:) = list of options for parameter values.
! integer          noptions    = number of options for parameter values.
! integer,optional    nchar    = maximum number of characters to encode.
! integer,optional     ndec    = maximum number of decimals to encode.
! logical         sensitive    = "sensitivity" flag (true or false).
! logical           visible    = "visibility" flag (true or false).
! integer           minsize    = minimum allowed number of array elements.
! integer           maxsize    = maximum allowed number of array elements.
! integer              indx    = array index to jump to.
!
!                     +++++++++++++++++++++++++++
!
! PC_REGISTER_ARRAY_NAMES:
!
!   The names of all the arrays in a set of linked arrays can optionally be
!   registered with the parameter cache in order to allow the parameter cache
!   to do some work for you.  This will simplify the treatment of linked arrays
!   by making sure they are all the same length.  You also must register the
!   names if you will be calling the subroutines PC_PUT_MINSIZE_ARRAYSET or
!   PC_PUT_MAXSIZE_ARRAYSET.
!
!   If NARRAYS is omitted, the size of ARRAYS(:) is assumed.
!
! PC_PUT_OPTIONS_FIELD AND PC_PUT_OPTIONS_ARRAY:
!
!   The type of OPTIONS can be real, integer, double precision, logical, or
!   character(len=*), and should match the type of the associated parameter.
!
!   If NOPTIONS is omitted, the size of OPTIONS(:) is assumed.
!
!   The NCHAR argument is for integer, real, and double precision only.
!   The NDEC argument is for real and double precision variables only.
!   No maximum restrictions are imposed if NCHAR and NDEC are not specified.
!
!   It is not necessary to call PC_PUT_OPTIONS_FIELD or PC_PUT_OPTIONS_ARRAY
!   for logical variables because this is done automatically by the
!   corresponding PC_PUT routines.  In such a case there are always exactly
!   two options (true and false) which show up as YES or NO in the GUI.
!
!   The options are to be used by a GUI to display as an option menu or a set
!   of radio buttons, or otherwise to restrict data entry to be one of these
!   allowed values.  However, there is no requirement that a GUI must use this
!   information.  Therefore, a process module still has a responsibility to
!   do its own error checking.
!
!   Currently, the PC_PUT_OPTIONS_ARRAY routine should not be used because
!   our CFE GUI does not allow a list of options for array elements.
!
! SENSITIVITY AND VISIBILITY:
!
!   SENSITIVE == .true.  means the GUI field is active (sensitive and editable).
!   SENSITIVE == .false. means the GUI field is inactive and grayed out.
!
!   The "sensitivity" flag is irrelevant for fields which are to be displayed
!   as informational.  Informational fields are inactive but not grayed out,
!   and have a different (more label-like) appearance than other fields.
!
!   VISIBLE == .true.  means the GUI field is visible (default).
!   VISIBLE == .false. means the GUI field is invisible.
!
! MINIMUM AND MAXIMUM ARRAY SIZES:
!
!   The minimum and and maximum number of array elements can be restricted
!   by calling the routines with the MINSIZE and MAXSIZE arguments.  If
!   MINSIZE is not reported, it is considered zero.  If MAXSIZE is not
!   reported, it is considered infinite.  These arguments restrict what happens
!   in the GUI, but have no effect otherwise.  Any change made by the user in
!   the GUI will not change the array lengths to go outside of the specified
!   bounds.  If the array length is less than MAXSIZE, there will be a blank
!   row appended in the GUI, and it will be possible to insert a row.  If the
!   array length is greated than MINSIZE, it will be possible to delete a row.
!
!   For example:
!    (1) If you must have at least one array element, set MINSIZE to one.
!    (2) If your array has a hard-wired length, set MINSIZE to this length.
!    (3) If the user is not allowed to insert or delete, set MINSIZE and
!         MAXSIZE to the current array length.
!
!   In all cases, the currently reported array length should not be less than
!   the reported MINSIZE or greater than the reported MAXSIZE.
!
!   For linked arrays, you can call the MINSIZE and/or MAXSIZE routines for
!   each array, or make a single call for the entire set of linked arrays.
!   In the latter case, you must also have previously called the subroutine
!   PC_REGISTER_ARRAY_NAMES for this arrayset.
!
! PC_JUMP ROUTINES:
!
!   The field jumped to by PC_JUMP... may be on the same or different screen.
!   PC_JUMP... can specify the current scalar keyword to remain in that field,
!   or the current array to remain in that array, or the current arrayset to
!   remain in that arrayset, or the current screen to remain in that screen.
!
!-------------------------------------------------------------------------------
!</calling_doc>



!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 34. 2008-12-11  B. Menger  Nullified even more pointers.
! 33. 2007-09-18  Stoeckley  Add ability to get variable types.
! 32. 2007-04-24  Stoeckley  Allow printing even if the parameter cache is not
!                             initialized; suppress even error and warning
!                             messages if the input LUN is negative; let
!                             pc_do_not_process_traces() return true if the
!                             parameter cache is not initialized.
! 31. 2006-10-23  Stoeckley  Remove pc_register_tab_group since now using the
!                             HelpSection instead.
! 30. 2006-09-11  D. Glover  Added NULLIFY statements for Intel compiler.
! 29. 2006-06-20  Stoeckley  Add pc_register_tab_group for SeisSpace.
! 28. 2006-06-08  Stoeckley  Add code to put arraysets into the workfile.
! 27. 2006-06-01  Stoeckley  Add code to pc_pressed to let SeisSpace know this
!                             is a button.
!026. 2006-04-25  B. Menger   Removed Unused Variables.
! 25. 2003-06-05  Stoeckley  Add code to to also print error and warning
!                             messages to standard out if LUN is not 6; add
!                             a discussion of the process module update routine
!                             and when traps are called.
! 24. 2002-06-24  Stoeckley  Add code to PC_REGISTER_ARRAY_NAMES to check
!                             for unequal lengths of linked arrays; make
!                             the NARRAYS and NOPTIONS arguments to
!                             PC_REGISTER_ARRAY_NAMES, PC_PUT_OPTIONS_FIELD,
!                             and PC_PUT_OPTIONS_ARRAY optional; add function
!                             and subroutine names to the "end subroutine" and
!                             "end function" cards for approximately 50
!                             functions and 370 subroutines; remove
!                             unnecessary IMPLICIT NONE and RETURN statements.
! 23. 2001-04-06  Stoeckley  Remove code which ignored duplicate messages.
! 22. 2001-01-09  C.C.Burch  Add the update state PC_BACKEND_NO_EXEC, plus
!                             the subroutines PC_SET_BACKEND_NO_EXEC and
!                             PC_SET_BACKEND_YES_EXEC, to help facilitate
!                             parallel processing on multiple CPU's.
! 21. 2000-09-15  Stoeckley  Add routines PC_ACTIVATED and PC_QUICK_UPDATE;
!                             add a new update state PC_QUICK.
! 20. 2000-08-21  Stoeckley  Speed up some of the code somewhat.
! 19. 2000-08-07  Stoeckley  Add capability of making GUI fields invisible;
!                             change print* statements to write(lunstop,*);
!                             add PC_EXISTS to allow determining whether
!                             the parameter cache is in an initialized state;
!                             add abort printout to indicate when an attempt
!                             is made to use the parameter cache without
!                             initializing it.
! 18. 2000-06-27  Stoeckley  Add automatic calls in PC_PUT to register option
!                             menus for logical variables; add error message
!                             and return of reasonable value in PC_GET_GLOBAL
!                             calls when the global is not present; remove
!                             accidental call to pc_private_assert.
! 17. 2000-05-17  Stoeckley  Add routines to report minimum and maximum
!                             array sizes and to register array names.
! 16. 2000-04-07  Stoeckley  Fix bug in previous change to PC_RESTORE.
! 15. 2000-03-31  Stoeckley  Change PC_RESTORE to save messages if preceded by
!                             PC_CLEAR.
! 14. 2000-03-21  Stoeckley  Add PC_CONTINUE_BACKEND_UPDATE to allow more than
!                             one loop in a job; add routines to print data
!                             cards or put them into informational messages.
! 13. 2000-03-10  Stoeckley  Add optional parameter to PC_ERROR (and similar)
!                             routines to exploit full capability in the
!                             STRING module.
! 12. 2000-02-07  Stoeckley  Change action LeavingArray to LeaveArray (and
!                             likewise for arrayset and screen).
! 11. 2000-02-04  Stoeckley  Remove obsolete action ModifyIndex and call
!                             cardset_remove_keyword instead of
!                             cardset_reset_keyword; add intentional aborts
!                             when attempts are made to alter project data
!                             and job data parameters when IPN is not 1 or
!                             2 respectively.  Also add code to generate
!                             automatic error messages when attempts are made
!                             to get missing global, project data, and job data
!                             parameters, but comment out this code until
!                             a decision is made to do this.
! 10. 2000-01-28  Stoeckley  Increase length of character variables, but
!                             retain data card length at same value; add code
!                             to split long variables between data cards;
!                             improve trap documentation; expand the permitted
!                             list of routines which can be called during
!                             trace processing.
!  9. 2000-01-24  Stoeckley  Add logical functions PC_VERIFY; change
!                             trap-calling criteria to always call a trap
!                             if a user retypes a value in a GUI even if
!                             the value does not change; change many arguments
!                             from intent(inout) to intent(out); change
!                             PC_ALLOC routines to always allocate at least
!                             one array element; add additional options
!                             reporting and message reporting routines for
!                             character and logical types.
!  8. 2000-01-13  Stoeckley  Add intentional aborts when certain routines
!                             are called illegally during trace processing,
!                             fix problem with calling scalar traps at the
!                             right time.
!  7. 2000-01-07  Stoeckley  Add PC_PRESSED and PC_BACKEND_EXECUTE.
!  6. 2000-01-07  Stoeckley  Add argument LUN to PC_FRONTEND_UPDATE and
!                             PC_BACKEND_UPDATE, and add routines to remove
!                             individual keywords.
!  5. 1999-12-21  Stoeckley  Add additional printouts when printing error
!                             messages for easier location in report files.
!  4. 1999-11-18  Stoeckley  Add ident string for RCS.  Also add optional
!                             arguments NCHAR and NDEC to several PC_PUT
!                             routines.  These were previously in the
!                             documentation but accidentally not implemented.
!                             Change action ReplaceArray to ReplaceElements.
!  3. 1999-09-20  Stoeckley  Incorporate GUI-related changes made by Donna
!                             Vunderink; consolidate redundant code; remove
!                             pc_value_changed and pc_get_old_value since
!                             this functionality is deficient and redundant.
!  2. 1999-09-10  Stoeckley  Changed spelling of public named constants.
!  1. 1999-06-25  Stoeckley  Initial version.
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



!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!
!!--------------------------- start of module -----------------------------!!


      module pc_module

      use cardset_module
      use grid_module
      use string_module
      use getlun_module
      use named_constants_module
      implicit none
      public
      private :: pc_private_startup
      private :: pc_private_create
      private :: pc_private_delete
      private :: pc_private_add_message
      private :: pc_private_error
      private :: pc_private_combine
      private :: pc_private_split
      private :: pc_private_erase
      private :: pc_private_call_trap
      private :: pc_private_call_etrap
      private :: pc_private_exists
      private :: pc_private_assert

      character(len=100),public,save :: PC_IDENT = &
'$Id: pc.f90,v 1.33 2007/09/19 14:02:25 Stoeckley beta sps $'


!!-------------------------- data structure ------------------------------!!
!!-------------------------- data structure ------------------------------!!
!!-------------------------- data structure ------------------------------!!


      type,private :: pc_struct

        type(cardset_struct),pointer :: pdata         ! cardset.
        type(cardset_struct),pointer :: jdata         ! cardset.
        type(cardset_struct),pointer :: global        ! cardset.
        type(cardset_struct),pointer :: process       ! cardset.
        type(cardset_struct),pointer :: control       ! cardset.
        type(cardset_struct),pointer :: gui           ! cardset.
        integer                      :: update_state  ! state variable.
        integer                      :: lun           ! state variable.
        integer                      :: ipn           ! state variable.
        logical                      :: prev_error    ! state variable.
        logical                      :: retain_messages
        type(pc_struct),pointer      :: previous      ! for linked list stack.

      end type pc_struct


!!---------------------------- data ---------------------------------------!!
!!---------------------------- data ---------------------------------------!!
!!---------------------------- data ---------------------------------------!!


   integer,parameter,public :: PC_FRONTEND        = 1   ! update state.
   integer,parameter,public :: PC_GUI             = 2   ! update state.
   integer,parameter,public :: PC_BACKEND         = 3   ! update state.
   integer,parameter,public :: PC_EXECUTE         = 4   ! update state.
   integer,parameter,public :: PC_QUICK           = 5   ! update state.
   integer,parameter,public :: PC_BACKEND_NO_EXEC = 6   ! update state

   integer,parameter,public :: PC_INSERT   = 1   ! for array element trap.
   integer,parameter,public :: PC_REMOVE   = 2   ! for array element trap.
   integer,parameter,public :: PC_MODIFY   = 3   ! for array element trap.
   integer,parameter,public :: PC_NOACTION = 4

   integer,parameter,public :: PC_DATACARD_LENGTH  = CARDSET_DATACARD_LENGTH
   integer,parameter,public :: PC_DATACARD_PADDING = CARDSET_DATACARD_PADDING
   integer,parameter,public :: PC_LENGTH  = CARDSET_LENGTH   ! char length.
   integer,parameter,public :: PC_MISSING = CARDSET_MISSING  ! nature.
   integer,parameter,public :: PC_SCALAR  = CARDSET_SCALAR   ! nature.
   integer,parameter,public :: PC_ARRAY   = CARDSET_ARRAY    ! nature.
   integer,parameter,public :: PC_INTEGER = CARDSET_INTEGER  ! vartype.
   integer,parameter,public :: PC_FLOAT   = CARDSET_FLOAT    ! vartype.
   integer,parameter,public :: PC_DOUBLE  = CARDSET_DOUBLE   ! vartype.
   integer,parameter,public :: PC_STRING  = CARDSET_STRING   ! vartype.
   integer,parameter,public :: PC_LOGICAL = CARDSET_LOGICAL  ! vartype.
   integer,parameter,public :: PC_GRID    = CARDSET_GRID     ! vartype.

   logical                ,private,save      :: starting = .true.
   type(pc_struct),pointer,private,save      :: obj
   integer                ,private,parameter :: LUNSTOP = 6

   integer,private,save :: discard    ! logical unit number for /dev/null.


!!------------------------- interfaces -------------------------------------!!
!!------------------------- interfaces -------------------------------------!!
!!------------------------- interfaces -------------------------------------!!


      interface pc_error
        module procedure pc_error_c
        module procedure pc_error_cici
        module procedure pc_error_cfcf
        module procedure pc_error_cdcd
        module procedure pc_error_cccc
        module procedure pc_error_clcl
      end interface


      interface pc_warning
        module procedure pc_warning_c
        module procedure pc_warning_cici
        module procedure pc_warning_cfcf
        module procedure pc_warning_cdcd
        module procedure pc_warning_cccc
        module procedure pc_warning_clcl
      end interface


      interface pc_info
        module procedure pc_info_c
        module procedure pc_info_cici
        module procedure pc_info_cfcf
        module procedure pc_info_cdcd
        module procedure pc_info_cccc
        module procedure pc_info_clcl
      end interface


      interface pc_print
        module procedure pc_print_c
        module procedure pc_print_cici
        module procedure pc_print_cfcf
        module procedure pc_print_cdcd
        module procedure pc_print_cccc
        module procedure pc_print_clcl
      end interface

                          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      interface pc_alloc
        module procedure pc_alloc_iarray
        module procedure pc_alloc_farray
        module procedure pc_alloc_darray
        module procedure pc_alloc_carray
        module procedure pc_alloc_larray
      end interface

      interface pc_alloc_process
        module procedure pc_alloc_process_iarray
        module procedure pc_alloc_process_farray
        module procedure pc_alloc_process_darray
        module procedure pc_alloc_process_carray
        module procedure pc_alloc_process_larray
      end interface

      interface pc_alloc_global
        module procedure pc_alloc_global_iarray
        module procedure pc_alloc_global_farray
        module procedure pc_alloc_global_darray
        module procedure pc_alloc_global_carray
        module procedure pc_alloc_global_larray
      end interface

      interface pc_alloc_control
        module procedure pc_alloc_control_iarray
        module procedure pc_alloc_control_farray
        module procedure pc_alloc_control_darray
        module procedure pc_alloc_control_carray
        module procedure pc_alloc_control_larray
      end interface

      interface pc_alloc_gui
        module procedure pc_alloc_gui_iarray
        module procedure pc_alloc_gui_farray
        module procedure pc_alloc_gui_darray
        module procedure pc_alloc_gui_carray
        module procedure pc_alloc_gui_larray
      end interface

      interface pc_alloc_pdata
        module procedure pc_alloc_pdata_iarray
        module procedure pc_alloc_pdata_farray
        module procedure pc_alloc_pdata_darray
        module procedure pc_alloc_pdata_carray
        module procedure pc_alloc_pdata_larray
      end interface

      interface pc_alloc_jdata
        module procedure pc_alloc_jdata_iarray
        module procedure pc_alloc_jdata_farray
        module procedure pc_alloc_jdata_darray
        module procedure pc_alloc_jdata_carray
        module procedure pc_alloc_jdata_larray
      end interface


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      interface pc_get
        module procedure pc_get_gscalar   ! grid transform
        module procedure pc_get_iscalar
        module procedure pc_get_fscalar
        module procedure pc_get_dscalar
        module procedure pc_get_cscalar
        module procedure pc_get_lscalar
        module procedure pc_get_iarray
        module procedure pc_get_farray
        module procedure pc_get_darray
        module procedure pc_get_carray
        module procedure pc_get_larray
      end interface

      interface pc_get_process
        module procedure pc_get_process_gscalar   ! grid transform
        module procedure pc_get_process_iscalar
        module procedure pc_get_process_fscalar
        module procedure pc_get_process_dscalar
        module procedure pc_get_process_cscalar
        module procedure pc_get_process_lscalar
        module procedure pc_get_process_iarray
        module procedure pc_get_process_farray
        module procedure pc_get_process_darray
        module procedure pc_get_process_carray
        module procedure pc_get_process_larray
        module procedure pc_get_process_ielement
        module procedure pc_get_process_felement
        module procedure pc_get_process_delement
        module procedure pc_get_process_celement
        module procedure pc_get_process_lelement
      end interface

      interface pc_get_global
        module procedure pc_get_global_gscalar   ! grid transform
        module procedure pc_get_global_iscalar
        module procedure pc_get_global_fscalar
        module procedure pc_get_global_dscalar
        module procedure pc_get_global_cscalar
        module procedure pc_get_global_lscalar
        module procedure pc_get_global_iarray
        module procedure pc_get_global_farray
        module procedure pc_get_global_darray
        module procedure pc_get_global_carray
        module procedure pc_get_global_larray
        module procedure pc_get_global_ielement
        module procedure pc_get_global_felement
        module procedure pc_get_global_delement
        module procedure pc_get_global_celement
        module procedure pc_get_global_lelement
      end interface

      interface pc_get_control
        module procedure pc_get_control_gscalar   ! grid transform
        module procedure pc_get_control_iscalar
        module procedure pc_get_control_fscalar
        module procedure pc_get_control_dscalar
        module procedure pc_get_control_cscalar
        module procedure pc_get_control_lscalar
        module procedure pc_get_control_iarray
        module procedure pc_get_control_farray
        module procedure pc_get_control_darray
        module procedure pc_get_control_carray
        module procedure pc_get_control_larray
        module procedure pc_get_control_ielement
        module procedure pc_get_control_felement
        module procedure pc_get_control_delement
        module procedure pc_get_control_celement
        module procedure pc_get_control_lelement
      end interface

      interface pc_get_gui
        module procedure pc_get_gui_gscalar   ! grid transform
        module procedure pc_get_gui_iscalar
        module procedure pc_get_gui_fscalar
        module procedure pc_get_gui_dscalar
        module procedure pc_get_gui_cscalar
        module procedure pc_get_gui_lscalar
        module procedure pc_get_gui_iarray
        module procedure pc_get_gui_farray
        module procedure pc_get_gui_darray
        module procedure pc_get_gui_carray
        module procedure pc_get_gui_larray
        module procedure pc_get_gui_ielement
        module procedure pc_get_gui_felement
        module procedure pc_get_gui_delement
        module procedure pc_get_gui_celement
        module procedure pc_get_gui_lelement
      end interface

      interface pc_get_pdata
        module procedure pc_get_pdata_gscalar   ! grid transform
        module procedure pc_get_pdata_iscalar
        module procedure pc_get_pdata_fscalar
        module procedure pc_get_pdata_dscalar
        module procedure pc_get_pdata_cscalar
        module procedure pc_get_pdata_lscalar
        module procedure pc_get_pdata_iarray
        module procedure pc_get_pdata_farray
        module procedure pc_get_pdata_darray
        module procedure pc_get_pdata_carray
        module procedure pc_get_pdata_larray
        module procedure pc_get_pdata_ielement
        module procedure pc_get_pdata_felement
        module procedure pc_get_pdata_delement
        module procedure pc_get_pdata_celement
        module procedure pc_get_pdata_lelement
      end interface

      interface pc_get_jdata
        module procedure pc_get_jdata_gscalar   ! grid transform
        module procedure pc_get_jdata_iscalar
        module procedure pc_get_jdata_fscalar
        module procedure pc_get_jdata_dscalar
        module procedure pc_get_jdata_cscalar
        module procedure pc_get_jdata_lscalar
        module procedure pc_get_jdata_iarray
        module procedure pc_get_jdata_farray
        module procedure pc_get_jdata_darray
        module procedure pc_get_jdata_carray
        module procedure pc_get_jdata_larray
        module procedure pc_get_jdata_ielement
        module procedure pc_get_jdata_felement
        module procedure pc_get_jdata_delement
        module procedure pc_get_jdata_celement
        module procedure pc_get_jdata_lelement
      end interface

                          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      interface pc_put
        module procedure pc_put_gscalar   ! grid transform
        module procedure pc_put_iscalar
        module procedure pc_put_fscalar
        module procedure pc_put_dscalar
        module procedure pc_put_cscalar
        module procedure pc_put_lscalar
        module procedure pc_put_iarray
        module procedure pc_put_farray
        module procedure pc_put_darray
        module procedure pc_put_carray
        module procedure pc_put_larray
      end interface

      interface pc_put_process
        module procedure pc_put_process_gscalar   ! grid transform
        module procedure pc_put_process_iscalar
        module procedure pc_put_process_fscalar
        module procedure pc_put_process_dscalar
        module procedure pc_put_process_cscalar
        module procedure pc_put_process_lscalar
        module procedure pc_put_process_iarray
        module procedure pc_put_process_farray
        module procedure pc_put_process_darray
        module procedure pc_put_process_carray
        module procedure pc_put_process_larray
      end interface

      interface pc_put_global
        module procedure pc_put_global_gscalar   ! grid transform
        module procedure pc_put_global_iscalar
        module procedure pc_put_global_fscalar
        module procedure pc_put_global_dscalar
        module procedure pc_put_global_cscalar
        module procedure pc_put_global_lscalar
        module procedure pc_put_global_iarray
        module procedure pc_put_global_farray
        module procedure pc_put_global_darray
        module procedure pc_put_global_carray
        module procedure pc_put_global_larray
      end interface

      interface pc_put_control
        module procedure pc_put_control_gscalar   ! grid transform
        module procedure pc_put_control_iscalar
        module procedure pc_put_control_fscalar
        module procedure pc_put_control_dscalar
        module procedure pc_put_control_cscalar
        module procedure pc_put_control_lscalar
        module procedure pc_put_control_iarray
        module procedure pc_put_control_farray
        module procedure pc_put_control_darray
        module procedure pc_put_control_carray
        module procedure pc_put_control_larray
      end interface

      interface pc_put_gui
        module procedure pc_put_gui_gscalar   ! grid transform
        module procedure pc_put_gui_iscalar
        module procedure pc_put_gui_fscalar
        module procedure pc_put_gui_dscalar
        module procedure pc_put_gui_cscalar
        module procedure pc_put_gui_lscalar
        module procedure pc_put_gui_iarray
        module procedure pc_put_gui_farray
        module procedure pc_put_gui_darray
        module procedure pc_put_gui_carray
        module procedure pc_put_gui_larray
      end interface

      interface pc_put_gui_only
        module procedure pc_put_gui_only_gscalar   ! grid transform
        module procedure pc_put_gui_only_iscalar
        module procedure pc_put_gui_only_fscalar
        module procedure pc_put_gui_only_dscalar
        module procedure pc_put_gui_only_cscalar
        module procedure pc_put_gui_only_lscalar
        module procedure pc_put_gui_only_iarray
        module procedure pc_put_gui_only_farray
        module procedure pc_put_gui_only_darray
        module procedure pc_put_gui_only_carray
        module procedure pc_put_gui_only_larray
      end interface

      interface pc_put_pdata
        module procedure pc_put_pdata_gscalar   ! grid transform
        module procedure pc_put_pdata_iscalar
        module procedure pc_put_pdata_fscalar
        module procedure pc_put_pdata_dscalar
        module procedure pc_put_pdata_cscalar
        module procedure pc_put_pdata_lscalar
        module procedure pc_put_pdata_iarray
        module procedure pc_put_pdata_farray
        module procedure pc_put_pdata_darray
        module procedure pc_put_pdata_carray
        module procedure pc_put_pdata_larray
      end interface

      interface pc_put_jdata
        module procedure pc_put_jdata_gscalar   ! grid transform
        module procedure pc_put_jdata_iscalar
        module procedure pc_put_jdata_fscalar
        module procedure pc_put_jdata_dscalar
        module procedure pc_put_jdata_cscalar
        module procedure pc_put_jdata_lscalar
        module procedure pc_put_jdata_iarray
        module procedure pc_put_jdata_farray
        module procedure pc_put_jdata_darray
        module procedure pc_put_jdata_carray
        module procedure pc_put_jdata_larray
      end interface

                          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      interface pc_put_options_field
        module procedure pc_put_options_iscalar
        module procedure pc_put_options_fscalar
        module procedure pc_put_options_dscalar
        module procedure pc_put_options_cscalar
        module procedure pc_put_options_lscalar
      end interface

      interface pc_put_options_array
        module procedure pc_put_options_iarray
        module procedure pc_put_options_farray
        module procedure pc_put_options_darray
        module procedure pc_put_options_carray
        module procedure pc_put_options_larray
      end interface


!!------------------------------ end of data -----------------------------!!
!!------------------------------ end of data -----------------------------!!
!!------------------------------ end of data -----------------------------!!


      contains


!!----------------------------- exists -------------------------------------!!
!!----------------------------- exists -------------------------------------!!
!!----------------------------- exists -------------------------------------!!


      function pc_exists () result (exists)
      logical :: exists                          ! result

      if (starting) then
           exists = .false.
      else if (.not.associated(obj)) then
           exists = .false.
      else
           exists = .true.
      end if
      end function pc_exists


!!-------------------- private combine and split ---------------------------!!
!!-------------------- private combine and split ---------------------------!!
!!-------------------- private combine and split ---------------------------!!


      subroutine pc_private_combine (keyword, action,   bigword)
      character(len=*),intent(in)  :: keyword             ! argument
      character(len=*),intent(in)  :: action              ! argument
      character(len=*),intent(out) :: bigword             ! argument

      bigword = trim(keyword)//'#'//trim(action)
  !   call string_to_upper (bigword)    ! not necessary.
      end subroutine pc_private_combine



      subroutine pc_private_split (bigword,   keyword, action)
      character(len=*),intent(in)  :: bigword             ! argument
      character(len=*),intent(out) :: keyword             ! argument
      character(len=*),intent(out) :: action              ! argument
      integer                      :: indx                ! local

      indx = scan(bigword, '#')
      if (indx == 0) then
           keyword = bigword
           action  = ' '
      else
           keyword = bigword(1:indx-1)
           action  = bigword(indx+1:)
      end if
  !   call string_to_upper (keyword)    ! not necessary.
  !   call string_to_upper (action)     ! not necessary.
      end subroutine pc_private_split


!!----------------- private create and delete ------------------------------!!
!!----------------- private create and delete ------------------------------!!
!!----------------- private create and delete ------------------------------!!


      subroutine pc_private_create (obj)
      type(pc_struct),pointer :: obj         ! argument

      nullify (obj)
      allocate(obj)

      nullify (obj%pdata) ! jpa
      nullify (obj%jdata) ! jpa
      nullify (obj%global) ! jpa
      nullify (obj%process) ! jpa
      nullify (obj%control) ! jpa
      nullify (obj%gui) ! jpa
      nullify (obj%previous) ! jpa

      call cardset_create             (obj%pdata   )
      call cardset_create             (obj%jdata   )
      call cardset_create             (obj%global  )
      call cardset_create             (obj%process )
      call cardset_create             (obj%control )
      call cardset_create             (obj%gui     )
      call cardset_set_packing_option (obj%pdata  , CARDSET_PACKED)
      call cardset_set_packing_option (obj%jdata  , CARDSET_PACKED)
      call cardset_set_packing_option (obj%global , CARDSET_PACKED)
      call cardset_set_packing_option (obj%process, CARDSET_PACKED)
      call cardset_set_packing_option (obj%control, CARDSET_PACKED)
      call cardset_set_packing_option (obj%gui    , CARDSET_PACKED)
      obj%update_state    = PC_BACKEND
      obj%lun             = discard
      obj%ipn             = 0
      obj%prev_error      = .false.
      obj%retain_messages = .false.
      end subroutine pc_private_create



      subroutine pc_private_delete (obj)
      type(pc_struct),pointer :: obj         ! argument

      call cardset_delete   (obj%pdata   )
      call cardset_delete   (obj%jdata   )
      call cardset_delete   (obj%global  )
      call cardset_delete   (obj%process )
      call cardset_delete   (obj%control )
      call cardset_delete   (obj%gui     )
      deallocate(obj)
      end subroutine pc_private_delete


!!----------------------- private startup -------------------------------!!
!!----------------------- private startup -------------------------------!!
!!----------------------- private startup -------------------------------!!


      subroutine pc_private_startup (update_state, lun)
      integer,intent(in)          :: update_state       ! argument
      integer,intent(in),optional :: lun                ! argument
      type(pc_struct),pointer     :: previous           ! local

      if (starting) then
           call getlun (discard)
           open (discard, file = '/dev/null')
           nullify (obj)
           starting = .false.
      end if

      previous => obj
      call pc_private_create (obj)
      obj%previous => previous

      obj%update_state = update_state
      obj%lun          = discard
      obj%ipn          = 0
      obj%prev_error   = .false.

      if (present(lun)) then
           if (lun /= 0) obj%lun = lun
      end if
      end subroutine pc_private_startup


!!---------------- frontend and backend and gui update --------------------!!
!!---------------- frontend and backend and gui update --------------------!!
!!---------------- frontend and backend and gui update --------------------!!


      subroutine pc_frontend_update (lun)
      integer,intent(in),optional :: lun                ! argument

      call pc_private_startup (PC_FRONTEND,lun)
      end subroutine pc_frontend_update



      subroutine pc_backend_update (lun)
      integer,intent(in),optional :: lun                ! argument

      call pc_private_startup (PC_BACKEND,lun)
      end subroutine pc_backend_update



      subroutine pc_gui_update (lun)
      integer,intent(in),optional :: lun                ! argument

      call pc_private_startup (PC_GUI,lun)
      end subroutine pc_gui_update



      subroutine pc_quick_update (lun)
      integer,intent(in),optional :: lun                ! argument

      call pc_private_startup (PC_QUICK,lun)
      end subroutine pc_quick_update


!!------------------------------ clear ----------------------------------!!
!!------------------------------ clear ----------------------------------!!
!!------------------------------ clear ----------------------------------!!


      subroutine pc_clear
      type(pc_struct),pointer :: previous           ! local

      call pc_private_exists ('pc_clear')

      previous => obj
      call pc_private_create (obj)
      obj%previous => previous

      call cardset_copy (obj%previous%pdata  , obj%pdata)
      call cardset_copy (obj%previous%jdata  , obj%jdata)
      call cardset_copy (obj%previous%global , obj%global)

      obj%update_state    = previous%update_state
      obj%lun             = previous%lun
      obj%ipn             = previous%ipn
      obj%prev_error      = previous%prev_error
      obj%retain_messages = .true.

      end subroutine pc_clear


!!----------------------------- restore ---------------------------------!!
!!----------------------------- restore ---------------------------------!!
!!----------------------------- restore ---------------------------------!!


      subroutine pc_restore
      type(pc_struct)         ,pointer :: previous           ! local
      character(len=PC_LENGTH),pointer :: errors  (:)        ! local
      character(len=PC_LENGTH),pointer :: warnings(:)        ! local
      character(len=PC_LENGTH),pointer :: infos   (:)        ! local
      integer                          :: nerrors            ! local
      integer                          :: nwarnings          ! local
      integer                          :: ninfos,i           ! local
      logical                          :: retaining          ! local

      call pc_private_exists ('pc_restore')

      if (obj%retain_messages .and. associated(obj%previous)) then
           nullify (errors)
           nullify (warnings)
           nullify (infos)
           nullify (previous)
           nerrors   = 0
           nwarnings = 0
           ninfos    = 0
           call pc_alloc_gui ('error'  , 'error'  , errors  , nerrors  )
           call pc_alloc_gui ('warning', 'warning', warnings, nwarnings)
           call pc_alloc_gui ('info'   , 'info'   , infos   , ninfos   )
           retaining = .true.
      else
           retaining = .false.
      end if

      previous => obj%previous
      call pc_private_delete (obj)
      obj => previous

      if (retaining) then
           do i = 1,nerrors
                call pc_error   (errors  (i))
           end do
           do i = 1,nwarnings
                call pc_warning (warnings(i))
           end do
           do i = 1,ninfos  
                call pc_info    (infos   (i))
           end do
           if (associated(errors  )) deallocate (errors)
           if (associated(warnings)) deallocate (warnings)
           if (associated(infos   )) deallocate (infos)
      end if

      end subroutine pc_restore


!!----------------------------- next ------------------------------------!!
!!----------------------------- next ------------------------------------!!
!!----------------------------- next ------------------------------------!!


      subroutine pc_next

      call pc_private_exists ('pc_next')

      call cardset_clear              (obj%process )
      call cardset_clear              (obj%control )
      call cardset_clear              (obj%gui     )
      call cardset_set_packing_option (obj%process, CARDSET_PACKED)
      call cardset_set_packing_option (obj%control, CARDSET_PACKED)
      call cardset_set_packing_option (obj%gui    , CARDSET_PACKED)
      obj%ipn = obj%ipn + 1

      end subroutine pc_next


!!------------------------ backend execute ------------------------------!!
!!------------------------ backend execute ------------------------------!!
!!------------------------ backend execute ------------------------------!!

!!! 3/20/00 the globals are no longer cleared so that the output globals
!!! from the last process will be retained to be used as input globals into
!!! a possible second set of setups in case there are more than one loop
!!! in the job.  Even though the globals are not cleared, they are still
!!! inaccessible to processes as long as the update state is PC_EXECUTE.
!!! For the same reason, IPN is no longer cleared so that the first process
!!! in the second set of setups will get the correct IPN.


      subroutine pc_backend_execute

      call pc_private_exists ('pc_backend_execute')

      if (pc_do_not_process_traces()) then
        write(LUNSTOP,*) '--> Illegally calling pc_backend_execute'
        write(LUNSTOP,*) '--> when trace processing cannot commence.'
        write(LUNSTOP,*) '--> This is a programming error.'
        stop
      end if

!!!!  call cardset_clear              (obj%global  )
      call cardset_clear              (obj%process )
      call cardset_clear              (obj%control )
      call cardset_clear              (obj%gui     )
!!!!  call cardset_set_packing_option (obj%global , CARDSET_PACKED)
      call cardset_set_packing_option (obj%process, CARDSET_PACKED)
      call cardset_set_packing_option (obj%control, CARDSET_PACKED)
      call cardset_set_packing_option (obj%gui    , CARDSET_PACKED)
!!!!  obj%ipn          = 0
      obj%update_state = PC_EXECUTE

      end subroutine pc_backend_execute


!!--------------------- continue backend update ----------------------------!!
!!--------------------- continue backend update ----------------------------!!
!!--------------------- continue backend update ----------------------------!!


      subroutine pc_continue_backend_update

      call pc_private_exists ('pc_continue_backend_update')

      obj%update_state = PC_BACKEND

      if (pc_do_not_process_traces()) then
        write(LUNSTOP,*) '--> Illegally calling pc_continue_backend_update'
        write(LUNSTOP,*) '--> when trace processing cannot continue.'
        write(LUNSTOP,*) '--> This is a programming error.'
        stop
      end if

      end subroutine pc_continue_backend_update


!!------------------------ private exists ---------------------------------!!
!!------------------------ private exists ---------------------------------!!
!!------------------------ private exists ---------------------------------!!

! This routine aborts with a message if there is no instance of the
! parameter cache in use.

! This routine should be called from all routines which attempt to use the
! current instance of the parameter cache.  The code in this routine also
! resides in PC_PRIVATE_ASSERT; therefore this routine need be called only
! from routines which do not call PC_PRIVATE_ASSERT.

      subroutine pc_private_exists (routine)
      character(len=*),intent(in) :: routine

      if (.not.pc_exists()) then
           write(LUNSTOP,*) '--> Illegally calling ',trim(routine)
           write(LUNSTOP,*) '--> when the parameter cache is not initialized.'
           write(LUNSTOP,*) '--> This is a programming error.'
           stop
      end if
      end subroutine pc_private_exists


!!------------------------ private assert ---------------------------------!!
!!------------------------ private assert ---------------------------------!!
!!------------------------ private assert ---------------------------------!!

! This routine aborts with a message if the update state is PC_EXECUTE.

! This routine should be called from all routines which set any parameters
! or state variables, or get process parameters, global parameters, control
! parameters, gui parameters, or IPN.

! If the optional ALLOW_IPN parameter is present, this routine will also abort
! (for any update state) unless the IPN is the same as ALLOW_IPN.  The purpose
! is to disallow setting project data or job data parameters unless the IPN
! is 1 or 2 respectively. 

      subroutine pc_private_assert (routine, allow_ipn)
      character(len=*),intent(in) :: routine
      integer,optional,intent(in) :: allow_ipn

      if (.not.pc_exists()) then
        write(LUNSTOP,*) '--> Illegally calling ',trim(routine)
        write(LUNSTOP,*) '--> when the parameter cache is not initialized.'
        write(LUNSTOP,*) '--> This is a programming error.'
        stop
      else if (obj%update_state == PC_EXECUTE) then
        write(LUNSTOP,*) '--> Illegally calling ',trim(routine)
        write(LUNSTOP,*) '--> while processing traces.'
        write(LUNSTOP,*) '--> This is a programming error.'
        stop
      else if (present(allow_ipn)) then
        if (obj%ipn /= allow_ipn) then
             write(LUNSTOP,*) '--> Illegally calling ',trim(routine)
             write(LUNSTOP,*) '--> when IPN ',obj%ipn,' is not ',allow_ipn,'.'
             write(LUNSTOP,*) '--> This is a programming error.'
             stop
        end if
      end if
      end subroutine pc_private_assert


!!-------------------- get and set state variables -------------------------!!
!!-------------------- get and set state variables -------------------------!!
!!-------------------- get and set state variables -------------------------!!


      function pc_get_update_state () result (update_state)
      integer :: update_state                   ! result

      call pc_private_exists ('pc_get_update_state')
      update_state = obj%update_state
      end function pc_get_update_state



      subroutine pc_set_backend_no_exec

      call pc_private_exists ('pc_set_backend_no_exec')
      if (obj%update_state /= PC_BACKEND) then
        write(LUNSTOP,*) '--> Illegally calling pc_set_backend_no_exec'
        write(LUNSTOP,*) '--> when the update state is not PC_BACKEND.'
        write(LUNSTOP,*) '--> This is a programming error.'
        stop
      end if
      obj%update_state = PC_BACKEND_NO_EXEC                    
      end subroutine pc_set_backend_no_exec           



      subroutine pc_set_backend_yes_exec

      call pc_private_exists ('pc_set_backend_yes_exec')
      if (obj%update_state /= PC_BACKEND_NO_EXEC) then
        write(LUNSTOP,*) '--> Illegally calling pc_set_backend_yes_exec'
        write(LUNSTOP,*) '--> when the update state is not PC_BACKEND_NO_EXEC.'
        write(LUNSTOP,*) '--> This is a programming error.'
        stop
      end if
      obj%update_state = PC_BACKEND
      end subroutine pc_set_backend_yes_exec           



      function pc_get_lun () result (lun)
      integer :: lun                            ! result

      if (.not.pc_exists()) then
           lun = LUNSTOP
      else if (obj%lun > 0) then
           lun = obj%lun
      else if (obj%lun < 0) then
           lun = -obj%lun
      else
           lun = discard
      end if
      end function pc_get_lun



      function pc_get_ipn () result (ipn)
      integer :: ipn                            ! result

      call pc_private_assert ('pc_get_ipn')
      ipn = obj%ipn
      end function pc_get_ipn



      function pc_previous_error () result (error)
      logical :: error                          ! result

      call pc_private_exists ('pc_previous_error')
      error = obj%prev_error
      end function pc_previous_error



      subroutine pc_set_ipn (ipn)
      integer,intent(in) :: ipn                 ! argument

      call pc_private_assert ('pc_set_ipn')
      obj%ipn = ipn
      end subroutine pc_set_ipn



!!------------------------- do not process traces ------------------------!!
!!------------------------- do not process traces ------------------------!!
!!------------------------- do not process traces ------------------------!!


      function pc_do_not_process_traces () result (stop_now)
      logical                     :: stop_now        ! result

      if (.not.pc_exists()) then
        write(LUNSTOP,*) '--> Illegally calling pc_do_not_process_traces'
        write(LUNSTOP,*) '--> when the parameter cache is not initialized.'
        write(LUNSTOP,*) '--> This is a programming error.'
        write(LUNSTOP,*) '--> This error causes pc_do_not_process_traces'
        write(LUNSTOP,*) '--> to return true (previously it aborted).'
        stop_now = .true.
        return
      end if
      stop_now = (obj%prev_error                         .or.  &
                  obj%update_state == PC_FRONTEND        .or.  &
                  obj%update_state == PC_QUICK           .or.  &
                  obj%update_state == PC_GUI             .or.  &
                  obj%update_state == PC_BACKEND_NO_EXEC .or.  &
                  pc_update_error())
      end function pc_do_not_process_traces


!!--------------------- deal with errors and messages --------------------!!
!!--------------------- deal with errors and messages --------------------!!
!!--------------------- deal with errors and messages --------------------!!


      function pc_update_error () result (error)
      logical :: error                       ! result
      integer :: num                         ! local

      call pc_private_exists    ('pc_update_error')
      num = pc_num_elements_gui ('error', 'error')
      error = (num > 0)
      end function pc_update_error


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_private_add_message (keyword, action, msg1)
      character(len=*),intent(in) :: keyword             ! argument
      character(len=*),intent(in) :: action              ! argument
      character(len=*),intent(in) :: msg1                ! argument

      character(len=PC_LENGTH)    :: bigword             ! local

      if (msg1 == ' ') return
      call pc_private_combine  (keyword, action, bigword)
      if (keyword == 'error') then
           call pc_print_c          ('++++++++++fatal error+++++++++++')
           call pc_print_c          ('++++++++++fatal error+++++++++++')
           call pc_print_c          (msg1)
           call pc_print_c          ('++++++++++fatal error+++++++++++')
           call pc_print_c          ('++++++++++fatal error+++++++++++')
      else
           call pc_print_c          (msg1)
      end if
      if (pc_exists()) then
           call cardset_add_element (obj%gui, bigword, msg1)
           if (obj%lun /= LUNSTOP .and. obj%lun >= 0) then
                if (keyword == 'error' .or. keyword == 'warning') then
                     write(LUNSTOP,*) trim(msg1)
                end if
           end if
      else
        write(LUNSTOP,*) '--> Illegally calling pc_'//action
        write(LUNSTOP,*) '--> when the parameter cache is not initialized.'
        write(LUNSTOP,*) '--> This is a programming error.'
        write(LUNSTOP,*) '--> This error is ignored.'
        write(LUNSTOP,*) '--> Previously this error caused an abort.'
      end if
      end subroutine pc_private_add_message


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!

! The following routine normally does not register an error if the error
! is simply that the keyword was not found.  However, such a message
! may be registered if the second (optional) argument is present.
! The second (optional) argument has the following values:
!   get pdata (1), get jdata (2), and get global (3).

      subroutine pc_private_error (msg1,getting)
      character(len=*),intent(in) :: msg1                ! argument
      integer,optional,intent(in) :: getting             ! argument

      if (msg1(1:8) == 'keyword ') then
           if (.not.present(getting)) return
           if (getting /= 3) return     ! return if not getting global.
      end if
      call pc_error_c (msg1)
      end subroutine pc_private_error


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_error_c (msg1)
      character(len=*),intent(in) :: msg1                ! argument

      call pc_private_add_message ('error', 'error', msg1)
      if (msg1 /= ' ') obj%prev_error = .true.
      end subroutine pc_error_c


      subroutine pc_error_cici (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      integer         ,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      integer         ,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cici (buffer,msg1,var1,msg2,var2,msg3)
      call pc_error_c         (buffer)
      end subroutine pc_error_cici


      subroutine pc_error_cfcf (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      real            ,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      real            ,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cfcf (buffer,msg1,var1,msg2,var2,msg3)
      call pc_error_c         (buffer)
      end subroutine pc_error_cfcf


      subroutine pc_error_cdcd (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      double precision,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      double precision,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cdcd (buffer,msg1,var1,msg2,var2,msg3)
      call pc_error_c         (buffer)
      end subroutine pc_error_cdcd


      subroutine pc_error_cccc (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      character(len=*),intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      character(len=*),intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cccc (buffer,msg1,var1,msg2,var2,msg3)
      call pc_error_c         (buffer)
      end subroutine pc_error_cccc


      subroutine pc_error_clcl (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      logical         ,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      logical         ,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_clcl (buffer,msg1,var1,msg2,var2,msg3)
      call pc_error_c         (buffer)
      end subroutine pc_error_clcl


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_warning_c (msg1)
      character(len=*),intent(in) :: msg1                ! argument

      call pc_private_add_message ('warning', 'warning', msg1)
      end subroutine pc_warning_c


      subroutine pc_warning_cici (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      integer         ,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      integer         ,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cici (buffer,msg1,var1,msg2,var2,msg3)
      call pc_warning_c       (buffer)
      end subroutine pc_warning_cici


      subroutine pc_warning_cfcf (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      real            ,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      real            ,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cfcf (buffer,msg1,var1,msg2,var2,msg3)
      call pc_warning_c       (buffer)
      end subroutine pc_warning_cfcf


      subroutine pc_warning_cdcd (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      double precision,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      double precision,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cdcd (buffer,msg1,var1,msg2,var2,msg3)
      call pc_warning_c       (buffer)
      end subroutine pc_warning_cdcd


      subroutine pc_warning_cccc (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      character(len=*),intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      character(len=*),intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cccc (buffer,msg1,var1,msg2,var2,msg3)
      call pc_warning_c       (buffer)
      end subroutine pc_warning_cccc


      subroutine pc_warning_clcl (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      logical         ,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      logical         ,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_clcl (buffer,msg1,var1,msg2,var2,msg3)
      call pc_warning_c       (buffer)
      end subroutine pc_warning_clcl

                          !!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine pc_info_c (msg1)
      character(len=*),intent(in) :: msg1                ! argument

      call pc_private_add_message ('info', 'info', msg1)
      end subroutine pc_info_c


      subroutine pc_info_cici (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      integer         ,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      integer         ,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cici (buffer,msg1,var1,msg2,var2,msg3)
      call pc_info_c          (buffer)
      end subroutine pc_info_cici


      subroutine pc_info_cfcf (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      real            ,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      real            ,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cfcf (buffer,msg1,var1,msg2,var2,msg3)
      call pc_info_c          (buffer)
      end subroutine pc_info_cfcf


      subroutine pc_info_cdcd (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      double precision,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      double precision,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cdcd (buffer,msg1,var1,msg2,var2,msg3)
      call pc_info_c          (buffer)
      end subroutine pc_info_cdcd


      subroutine pc_info_cccc (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      character(len=*),intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      character(len=*),intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cccc (buffer,msg1,var1,msg2,var2,msg3)
      call pc_info_c          (buffer)
      end subroutine pc_info_cccc


      subroutine pc_info_clcl (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      logical         ,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      logical         ,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_clcl (buffer,msg1,var1,msg2,var2,msg3)
      call pc_info_c          (buffer)
      end subroutine pc_info_clcl


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!! On multiple-cpu machines, PC_PRINT_C should print only from a
!!!! specified CPU (which might be the "root" or "boss" CPU), except
!!!! for error and warning messages, which should always be printed.

!!!! We need a print routine which also prints an array.


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_print_c (msg1)
      character(len=*),intent(in) :: msg1                ! argument

      if (.not.pc_exists()) then
           write(LUNSTOP,*) trim(msg1)
      else if (obj%lun > 0) then
           write(obj%lun,*) trim(msg1)
      end if
      end subroutine pc_print_c


      subroutine pc_print_cici (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      integer         ,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      integer         ,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cici (buffer,msg1,var1,msg2,var2,msg3)
      call pc_print_c         (buffer)
      end subroutine pc_print_cici


      subroutine pc_print_cfcf (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      real            ,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      real            ,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cfcf (buffer,msg1,var1,msg2,var2,msg3)
      call pc_print_c         (buffer)
      end subroutine pc_print_cfcf


      subroutine pc_print_cdcd (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      double precision,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      double precision,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cdcd (buffer,msg1,var1,msg2,var2,msg3)
      call pc_print_c         (buffer)
      end subroutine pc_print_cdcd


      subroutine pc_print_cccc (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      character(len=*),intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      character(len=*),intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_cccc (buffer,msg1,var1,msg2,var2,msg3)
      call pc_print_c         (buffer)
      end subroutine pc_print_cccc


      subroutine pc_print_clcl (msg1,var1,msg2,var2,msg3)
      character(len=*),intent(in)          :: msg1       ! argument
      logical         ,intent(in)          :: var1       ! argument
      character(len=*),intent(in),optional :: msg2       ! argument
      logical         ,intent(in),optional :: var2       ! argument
      character(len=*),intent(in),optional :: msg3       ! argument
      character(len=PC_LENGTH)             :: buffer     ! local

      call string_encode_clcl (buffer,msg1,var1,msg2,var2,msg3)
      call pc_print_c         (buffer)
      end subroutine pc_print_clcl


!!------------------------ print data cards ----------------------------!!
!!------------------------ print data cards ----------------------------!!
!!------------------------ print data cards ----------------------------!!


      subroutine pc_print_process_cards
      character(len=PC_DATACARD_LENGTH) :: card                ! local
      integer                           :: ncards,i            ! local
      character(len=8)                  :: errmsg              ! local

      call pc_private_exists ('pc_print_process_cards')
      ncards = pc_num_process_cards()
      do i = 1,ncards
           call pc_get_process_card (i,card,errmsg)
           call pc_print            ('   '//card)
      end do
      end subroutine pc_print_process_cards



      subroutine pc_print_global_cards
      character(len=PC_DATACARD_LENGTH) :: card                ! local
      integer                           :: ncards,i            ! local
      character(len=8)                  :: errmsg              ! local

      call pc_private_exists ('pc_print_global_cards')
      ncards = pc_num_global_cards()
      do i = 1,ncards
           call pc_get_global_card (i,card,errmsg)
           call pc_print           ('   '//card)
      end do
      end subroutine pc_print_global_cards



      subroutine pc_print_control_cards
      character(len=PC_DATACARD_LENGTH) :: card                ! local
      integer                           :: ncards,i            ! local
      character(len=8)                  :: errmsg              ! local

      call pc_private_exists ('pc_print_control_cards')
      ncards = pc_num_control_cards()
      do i = 1,ncards
           call pc_get_control_card (i,card,errmsg)
           call pc_print            ('   '//card)
      end do
      end subroutine pc_print_control_cards



      subroutine pc_print_pdata_cards
      character(len=PC_DATACARD_LENGTH) :: card                ! local
      integer                           :: ncards,i            ! local
      character(len=8)                  :: errmsg              ! local

      call pc_private_exists ('pc_print_pdata_cards')
      ncards = pc_num_pdata_cards()
      do i = 1,ncards
           call pc_get_pdata_card (i,card,errmsg)
           call pc_print          ('   '//card)
      end do
      end subroutine pc_print_pdata_cards



      subroutine pc_print_jdata_cards
      character(len=PC_DATACARD_LENGTH) :: card                ! local
      integer                           :: ncards,i            ! local
      character(len=8)                  :: errmsg              ! local

      call pc_private_exists ('pc_print_jdata_cards')
      ncards = pc_num_jdata_cards()
      do i = 1,ncards
           call pc_get_jdata_card (i,card,errmsg)
           call pc_print          ('   '//card)
      end do
      end subroutine pc_print_jdata_cards



      subroutine pc_print_gui_cards
      character(len=PC_DATACARD_LENGTH) :: card                ! local
      integer                           :: ncards,i            ! local
      character(len=8)                  :: errmsg              ! local

      call pc_private_exists ('pc_print_gui_cards')
      ncards = pc_num_gui_cards()
      do i = 1,ncards
           call pc_get_gui_card (i,card,errmsg)
           call pc_print        ('   '//card)
      end do
      end subroutine pc_print_gui_cards

                              !!!!!!!!!!!!!!!!!!


      subroutine pc_info_process_cards
      character(len=PC_DATACARD_LENGTH) :: card                ! local
      integer                           :: ncards,i            ! local
      character(len=8)                  :: errmsg              ! local

      call pc_private_exists ('pc_info_process_cards')
      ncards = pc_num_process_cards()
      do i = 1,ncards
           call pc_get_process_card (i,card,errmsg)
           call pc_info             ('   '//card)
      end do
      end subroutine pc_info_process_cards



      subroutine pc_info_global_cards
      character(len=PC_DATACARD_LENGTH) :: card                ! local
      integer                           :: ncards,i            ! local
      character(len=8)                  :: errmsg              ! local

      call pc_private_exists ('pc_info_global_cards')
      ncards = pc_num_global_cards()
      do i = 1,ncards
           call pc_get_global_card (i,card,errmsg)
           call pc_info            ('   '//card)
      end do
      end subroutine pc_info_global_cards



      subroutine pc_info_control_cards
      character(len=PC_DATACARD_LENGTH) :: card                ! local
      integer                           :: ncards,i            ! local
      character(len=8)                  :: errmsg              ! local

      call pc_private_exists ('pc_info_control_cards')
      ncards = pc_num_control_cards()
      do i = 1,ncards
           call pc_get_control_card (i,card,errmsg)
           call pc_info             ('   '//card)
      end do
      end subroutine pc_info_control_cards



      subroutine pc_info_pdata_cards
      character(len=PC_DATACARD_LENGTH) :: card                ! local
      integer                           :: ncards,i            ! local
      character(len=8)                  :: errmsg              ! local

      call pc_private_exists ('pc_info_pdata_cards')
      ncards = pc_num_pdata_cards()
      do i = 1,ncards
           call pc_get_pdata_card (i,card,errmsg)
           call pc_info           ('   '//card)
      end do
      end subroutine pc_info_pdata_cards



      subroutine pc_info_jdata_cards
      character(len=PC_DATACARD_LENGTH) :: card                ! local
      integer                           :: ncards,i            ! local
      character(len=8)                  :: errmsg              ! local

      call pc_private_exists ('pc_info_jdata_cards')
      ncards = pc_num_jdata_cards()
      do i = 1,ncards
           call pc_get_jdata_card (i,card,errmsg)
           call pc_info           ('   '//card)
      end do
      end subroutine pc_info_jdata_cards



      subroutine pc_info_gui_cards
      character(len=PC_DATACARD_LENGTH) :: card                ! local
      integer                           :: ncards,i            ! local
      character(len=8)                  :: errmsg              ! local

      call pc_private_exists ('pc_info_gui_cards')
      ncards = pc_num_gui_cards()
      do i = 1,ncards
           call pc_get_gui_card (i,card,errmsg)
           call pc_info         ('   '//card)
      end do
      end subroutine pc_info_gui_cards



!!------------------------ num elements -----------------------------------!!
!!------------------------ num elements -----------------------------------!!
!!------------------------ num elements -----------------------------------!!


      function pc_num_elements_process (keyword) result (nelements)
      character(len=*) ,intent(in)  :: keyword           ! argument
      integer                       :: nelements         ! result

      call pc_private_assert           ('pc_num_elements_process')
      nelements = cardset_num_elements (obj%process, keyword)
      end function pc_num_elements_process


      function pc_num_elements_global (keyword) result (nelements)
      character(len=*) ,intent(in)  :: keyword           ! argument
      integer                       :: nelements         ! result

      call pc_private_assert           ('num_elements_global')
      nelements = cardset_num_elements (obj%global, keyword)
      end function pc_num_elements_global


      function pc_num_elements_control (keyword) result (nelements)
      character(len=*) ,intent(in)  :: keyword           ! argument
      integer                       :: nelements         ! result

      call pc_private_assert           ('pc_num_elements_control')
      nelements = cardset_num_elements (obj%control, keyword)
      end function pc_num_elements_control


      function pc_num_elements_gui (keyword, action) result (nelements)
      character(len=*) ,intent(in)  :: keyword           ! argument
      character(len=*) ,intent(in)  :: action            ! argument
      integer                       :: nelements         ! result
      character(len=PC_LENGTH)      :: bigword           ! local

      call pc_private_exists           ('pc_num_elements_gui')
!!!   call pc_private_assert           ('pc_num_elements_gui')
      call pc_private_combine          (keyword, action,   bigword)
      nelements = cardset_num_elements (obj%gui, bigword)
      end function pc_num_elements_gui


      function pc_num_elements_pdata (keyword) result (nelements)
      character(len=*) ,intent(in)  :: keyword           ! argument
      integer                       :: nelements         ! result

      call pc_private_exists           ('pc_num_elements_pdata')
      nelements = cardset_num_elements (obj%pdata, keyword)
      end function pc_num_elements_pdata


      function pc_num_elements_jdata (keyword) result (nelements)
      character(len=*) ,intent(in)  :: keyword           ! argument
      integer                       :: nelements         ! result

      call pc_private_exists           ('pc_num_elements_jdata')
      nelements = cardset_num_elements (obj%jdata, keyword)
      end function pc_num_elements_jdata


!!--------------------------- nature --------------------------------------!!
!!--------------------------- nature --------------------------------------!!
!!--------------------------- nature --------------------------------------!!


      function pc_nature_process (keyword) result (nature)
      character(len=*) ,intent(in)  :: keyword        ! argument
      integer                       :: nature         ! result

      call pc_private_assert  ('pc_nature_process')
      nature = cardset_nature (obj%process, keyword)
      end function pc_nature_process


      function pc_nature_global (keyword) result (nature)
      character(len=*) ,intent(in)  :: keyword        ! argument
      integer                       :: nature         ! result

      call pc_private_assert  ('pc_nature_global')
      nature = cardset_nature (obj%global, keyword)
      end function pc_nature_global


      function pc_nature_control (keyword) result (nature)
      character(len=*) ,intent(in)  :: keyword        ! argument
      integer                       :: nature         ! result

      call pc_private_assert  ('pc_nature_control')
      nature = cardset_nature (obj%control, keyword)
      end function pc_nature_control


      function pc_nature_gui (keyword, action) result (nature)
      character(len=*) ,intent(in)  :: keyword        ! argument
      character(len=*) ,intent(in)  :: action         ! argument
      integer                       :: nature         ! result
      character(len=PC_LENGTH)      :: bigword        ! local

      call pc_private_exists  ('pc_nature_gui')
!!!   call pc_private_assert  ('pc_nature_gui')
      call pc_private_combine (keyword, action,   bigword)
      nature = cardset_nature (obj%gui, bigword)
      end function pc_nature_gui


      function pc_nature_pdata (keyword) result (nature)
      character(len=*) ,intent(in)  :: keyword        ! argument
      integer                       :: nature         ! result

      call pc_private_exists  ('pc_nature_pdata')
      nature = cardset_nature (obj%pdata, keyword)
      end function pc_nature_pdata


      function pc_nature_jdata (keyword) result (nature)
      character(len=*) ,intent(in)  :: keyword        ! argument
      integer                       :: nature         ! result

      call pc_private_exists  ('pc_nature_jdata')
      nature = cardset_nature (obj%jdata, keyword)
      end function pc_nature_jdata


!!--------------------------- vartype ------------------------------------!!
!!--------------------------- vartype ------------------------------------!!
!!--------------------------- vartype ------------------------------------!!


      function pc_vartype_process (keyword) result (vartype)
      character(len=*) ,intent(in)  :: keyword        ! argument
      integer                       :: vartype        ! result

      call pc_private_assert  ('pc_vartype_process')
      vartype = cardset_vartype (obj%process, keyword)
      end function pc_vartype_process


      function pc_vartype_global (keyword) result (vartype)
      character(len=*) ,intent(in)  :: keyword        ! argument
      integer                       :: vartype        ! result

      call pc_private_assert  ('pc_vartype_global')
      vartype = cardset_vartype (obj%global, keyword)
      end function pc_vartype_global


      function pc_vartype_control (keyword) result (vartype)
      character(len=*) ,intent(in)  :: keyword        ! argument
      integer                       :: vartype        ! result

      call pc_private_assert  ('pc_vartype_control')
      vartype = cardset_vartype (obj%control, keyword)
      end function pc_vartype_control


      function pc_vartype_gui (keyword, action) result (vartype)
      character(len=*) ,intent(in)  :: keyword        ! argument
      character(len=*) ,intent(in)  :: action         ! argument
      integer                       :: vartype        ! result
      character(len=PC_LENGTH)      :: bigword        ! local

      call pc_private_exists  ('pc_vartype_gui')
!!!   call pc_private_assert  ('pc_vartype_gui')
      call pc_private_combine (keyword, action,   bigword)
      vartype = cardset_vartype (obj%gui, bigword)
      end function pc_vartype_gui


      function pc_vartype_pdata (keyword) result (vartype)
      character(len=*) ,intent(in)  :: keyword        ! argument
      integer                       :: vartype        ! result

      call pc_private_exists  ('pc_vartype_pdata')
      vartype = cardset_vartype (obj%pdata, keyword)
      end function pc_vartype_pdata


      function pc_vartype_jdata (keyword) result (vartype)
      character(len=*) ,intent(in)  :: keyword        ! argument
      integer                       :: vartype        ! result

      call pc_private_exists  ('pc_vartype_jdata')
      vartype = cardset_vartype (obj%jdata, keyword)
      end function pc_vartype_jdata


!!---------------------- get scalars --------------------------------------!!
!!---------------------- get scalars --------------------------------------!!
!!---------------------- get scalars --------------------------------------!!


      subroutine pc_get_gscalar (keyword,scalar,trap)
      character(len=*) ,intent(in)     :: keyword           ! argument
      type(grid_struct),intent(out)    :: scalar            ! argument
      external                            trap              ! argument
      optional                            trap              ! argument

      call pc_private_assert    ('pc_get')
      call pc_get_gui           (keyword, 'ModifyField', scalar)
      call pc_get_process       (keyword,                scalar)
      call pc_private_call_trap (keyword, trap)
      end subroutine pc_get_gscalar



      subroutine pc_get_iscalar (keyword,scalar,trap)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(out)    :: scalar            ! argument
      external                            trap              ! argument
      optional                            trap              ! argument

      call pc_private_assert    ('pc_get')
      call pc_get_gui           (keyword, 'ModifyField', scalar)
      call pc_get_process       (keyword,                scalar)
      call pc_private_call_trap (keyword, trap)
      end subroutine pc_get_iscalar



      subroutine pc_get_fscalar (keyword,scalar,trap)
      character(len=*) ,intent(in)     :: keyword           ! argument
      real             ,intent(out)    :: scalar            ! argument
      external                            trap              ! argument
      optional                            trap              ! argument

      call pc_private_assert    ('pc_get')
      call pc_get_gui           (keyword, 'ModifyField', scalar)
      call pc_get_process       (keyword,                scalar)
      call pc_private_call_trap (keyword, trap)
      end subroutine pc_get_fscalar



      subroutine pc_get_dscalar (keyword,scalar,trap)
      character(len=*) ,intent(in)     :: keyword           ! argument
      double precision ,intent(out)    :: scalar            ! argument
      external                            trap              ! argument
      optional                            trap              ! argument

      call pc_private_assert    ('pc_get')
      call pc_get_gui           (keyword, 'ModifyField', scalar)
      call pc_get_process       (keyword,                scalar)
      call pc_private_call_trap (keyword, trap)
      end subroutine pc_get_dscalar



      subroutine pc_get_lscalar (keyword,scalar,trap)
      character(len=*) ,intent(in)     :: keyword           ! argument
      logical          ,intent(out)    :: scalar            ! argument
      external                            trap              ! argument
      optional                            trap              ! argument

      call pc_private_assert    ('pc_get')
      call pc_get_gui           (keyword, 'ModifyField', scalar)
      call pc_get_process       (keyword,                scalar)
      call pc_private_call_trap (keyword, trap)
      end subroutine pc_get_lscalar



      subroutine pc_get_cscalar (keyword,scalar,trap)
      character(len=*) ,intent(in)     :: keyword           ! argument
      character(len=*) ,intent(out)    :: scalar            ! argument
      external                            trap              ! argument
      optional                            trap              ! argument

      call pc_private_assert    ('pc_get')
      call pc_get_gui           (keyword, 'ModifyField', scalar)
      call pc_get_process       (keyword,                scalar)
      call pc_private_call_trap (keyword, trap)
      end subroutine pc_get_cscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_process_gscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      type(grid_struct),intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_process')
      call cardset_get_scalar (obj%process, keyword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_process_gscalar



      subroutine pc_get_process_iscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_process')
      call cardset_get_scalar (obj%process, keyword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_process_iscalar



      subroutine pc_get_process_fscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      real             ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_process')
      call cardset_get_scalar (obj%process, keyword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_process_fscalar



      subroutine pc_get_process_dscalar (keyword,scalar)
      character(len=*) ,intent(in)         :: keyword           ! argument
      double precision ,intent(out)        :: scalar            ! argument
      character(len=PC_LENGTH)             :: errmsg            ! local

      call pc_private_assert  ('pc_get_process')
      call cardset_get_scalar (obj%process, keyword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_process_dscalar



      subroutine pc_get_process_lscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      logical          ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_process')
      call cardset_get_scalar (obj%process, keyword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_process_lscalar



      subroutine pc_get_process_cscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      character(len=*) ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_process')
      call cardset_get_scalar (obj%process, keyword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_process_cscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_global_gscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      type(grid_struct),intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_global')
      call cardset_get_scalar (obj%global, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,3)
      if (errmsg /= ' ') call grid_initialize (scalar)
      end subroutine pc_get_global_gscalar



      subroutine pc_get_global_iscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg,keyword2   ! local

      call pc_private_assert  ('pc_get_global')
      call cardset_get_scalar (obj%global, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,3)
!----------special fixup code:
      call string_to_upper (keyword,keyword2)
      if (errmsg /= ' ') scalar = 100
      if (keyword2 == 'NWIH') then
           if (scalar < HDR_NOMINAL_SIZE) then
                call pc_error ('PC: illegal input value for global',keyword2)
                scalar = HDR_NOMINAL_SIZE
           end if
      end if
      if (scalar <= 0) then
           call pc_error ('PC: illegal input value for global',keyword2)
           scalar = 1
      end if
      end subroutine pc_get_global_iscalar



      subroutine pc_get_global_fscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      real             ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg,keyword2   ! local

      call pc_private_assert  ('pc_get_global')
      call cardset_get_scalar (obj%global, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,3)
!----------special fixup code:
      call string_to_upper (keyword,keyword2)
      if (errmsg /= ' ') then
           scalar = 0.0
           if (keyword2 == 'DT') scalar = 0.004
      else if (keyword2 == 'DT') then
           if (scalar <= 0.0) then
                call pc_error ('PC: illegal input value for global',keyword2)
                scalar = 0.004
           end if
      end if
      end subroutine pc_get_global_fscalar



      subroutine pc_get_global_dscalar (keyword,scalar)
      character(len=*) ,intent(in)         :: keyword           ! argument
      double precision ,intent(out)        :: scalar            ! argument
      character(len=PC_LENGTH)             :: errmsg            ! local

      call pc_private_assert  ('pc_get_global')
      call cardset_get_scalar (obj%global, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,3)
      if (errmsg /= ' ') scalar = 0.0
      end subroutine pc_get_global_dscalar



      subroutine pc_get_global_lscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      logical          ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_global')
      call cardset_get_scalar (obj%global, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,3)
      if (errmsg /= ' ') scalar = .false.
      end subroutine pc_get_global_lscalar



      subroutine pc_get_global_cscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      character(len=*) ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_global')
      call cardset_get_scalar (obj%global, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,3)
      if (errmsg /= ' ') scalar = ' '       
      end subroutine pc_get_global_cscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_control_gscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      type(grid_struct),intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_control')
      call cardset_get_scalar (obj%control, keyword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_control_gscalar



      subroutine pc_get_control_iscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_control')
      call cardset_get_scalar (obj%control, keyword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_control_iscalar



      subroutine pc_get_control_fscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      real             ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_control')
      call cardset_get_scalar (obj%control, keyword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_control_fscalar



      subroutine pc_get_control_dscalar (keyword,scalar)
      character(len=*) ,intent(in)         :: keyword           ! argument
      double precision ,intent(out)        :: scalar            ! argument
      character(len=PC_LENGTH)             :: errmsg            ! local

      call pc_private_assert  ('pc_get_control')
      call cardset_get_scalar (obj%control, keyword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_control_dscalar



      subroutine pc_get_control_lscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      logical          ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_control')
      call cardset_get_scalar (obj%control, keyword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_control_lscalar



      subroutine pc_get_control_cscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      character(len=*) ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert  ('pc_get_control')
      call cardset_get_scalar (obj%control, keyword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_control_cscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_gui_gscalar (keyword,action,scalar)
      character(len=*) ,intent(in)           :: keyword           ! argument
      character(len=*) ,intent(in)           :: action            ! argument
      type(grid_struct),intent(out)          :: scalar            ! argument
      character(len=PC_LENGTH)               :: bigword           ! local
      character(len=PC_LENGTH)               :: errmsg            ! local

      call pc_private_assert  ('pc_get_gui')
      call pc_private_combine (keyword, action, bigword)
      call cardset_get_scalar (obj%gui, bigword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_gui_gscalar



      subroutine pc_get_gui_iscalar (keyword,action,scalar)
      character(len=*) ,intent(in)           :: keyword           ! argument
      character(len=*) ,intent(in)           :: action            ! argument
      integer          ,intent(out)          :: scalar            ! argument
      character(len=PC_LENGTH)               :: bigword           ! local
      character(len=PC_LENGTH)               :: errmsg            ! local

      call pc_private_assert  ('pc_get_gui')
      call pc_private_combine (keyword, action, bigword)
      call cardset_get_scalar (obj%gui, bigword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_gui_iscalar



      subroutine pc_get_gui_fscalar (keyword,action,scalar)
      character(len=*) ,intent(in)           :: keyword           ! argument
      character(len=*) ,intent(in)           :: action            ! argument
      real             ,intent(out)          :: scalar            ! argument
      character(len=PC_LENGTH)               :: bigword           ! local
      character(len=PC_LENGTH)               :: errmsg            ! local

      call pc_private_assert  ('pc_get_gui')
      call pc_private_combine (keyword, action, bigword)
      call cardset_get_scalar (obj%gui, bigword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_gui_fscalar



      subroutine pc_get_gui_dscalar (keyword,action,scalar)
      character(len=*) ,intent(in)           :: keyword           ! argument
      character(len=*) ,intent(in)           :: action            ! argument
      double precision ,intent(out)          :: scalar            ! argument
      character(len=PC_LENGTH)               :: bigword           ! local
      character(len=PC_LENGTH)               :: errmsg            ! local

      call pc_private_assert  ('pc_get_gui')
      call pc_private_combine (keyword, action, bigword)
      call cardset_get_scalar (obj%gui, bigword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_gui_dscalar



      subroutine pc_get_gui_lscalar (keyword,action,scalar)
      character(len=*) ,intent(in)           :: keyword           ! argument
      character(len=*) ,intent(in)           :: action            ! argument
      logical          ,intent(out)          :: scalar            ! argument
      character(len=PC_LENGTH)               :: bigword           ! local
      character(len=PC_LENGTH)               :: errmsg            ! local

      call pc_private_assert  ('pc_get_gui')
      call pc_private_combine (keyword, action, bigword)
      call cardset_get_scalar (obj%gui, bigword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_gui_lscalar



      subroutine pc_get_gui_cscalar (keyword,action,scalar)
      character(len=*) ,intent(in)           :: keyword           ! argument
      character(len=*) ,intent(in)           :: action            ! argument
      character(len=*) ,intent(out)          :: scalar            ! argument
      character(len=PC_LENGTH)               :: bigword           ! local
      character(len=PC_LENGTH)               :: errmsg            ! local

!!!   call pc_private_assert  ('pc_get_gui')
      call pc_private_combine (keyword, action, bigword)
      call cardset_get_scalar (obj%gui, bigword, scalar, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_gui_cscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_pdata_gscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      type(grid_struct),intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists  ('pc_get_pdata')
      call cardset_get_scalar (obj%pdata, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,1)
      end subroutine pc_get_pdata_gscalar



      subroutine pc_get_pdata_iscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists  ('pc_get_pdata')
      call cardset_get_scalar (obj%pdata, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,1)
      end subroutine pc_get_pdata_iscalar



      subroutine pc_get_pdata_fscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      real             ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists  ('pc_get_pdata')
      call cardset_get_scalar (obj%pdata, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,1)
      end subroutine pc_get_pdata_fscalar



      subroutine pc_get_pdata_dscalar (keyword,scalar)
      character(len=*) ,intent(in)         :: keyword           ! argument
      double precision ,intent(out)        :: scalar            ! argument
      character(len=PC_LENGTH)             :: errmsg            ! local

      call pc_private_exists  ('pc_get_pdata')
      call cardset_get_scalar (obj%pdata, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,1)
      end subroutine pc_get_pdata_dscalar



      subroutine pc_get_pdata_lscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      logical          ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists  ('pc_get_pdata')
      call cardset_get_scalar (obj%pdata, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,1)
      end subroutine pc_get_pdata_lscalar



      subroutine pc_get_pdata_cscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      character(len=*) ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists  ('pc_get_pdata')
      call cardset_get_scalar (obj%pdata, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,1)
      end subroutine pc_get_pdata_cscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_jdata_gscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      type(grid_struct),intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists  ('pc_get_jdata')
      call cardset_get_scalar (obj%jdata, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,2)
      end subroutine pc_get_jdata_gscalar



      subroutine pc_get_jdata_iscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists  ('pc_get_jdata')
      call cardset_get_scalar (obj%jdata, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,2)
      end subroutine pc_get_jdata_iscalar



      subroutine pc_get_jdata_fscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      real             ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists  ('pc_get_jdata')
      call cardset_get_scalar (obj%jdata, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,2)
      end subroutine pc_get_jdata_fscalar



      subroutine pc_get_jdata_dscalar (keyword,scalar)
      character(len=*) ,intent(in)         :: keyword           ! argument
      double precision ,intent(out)        :: scalar            ! argument
      character(len=PC_LENGTH)             :: errmsg            ! local

      call pc_private_exists  ('pc_get_jdata')
      call cardset_get_scalar (obj%jdata, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,2)
      end subroutine pc_get_jdata_dscalar



      subroutine pc_get_jdata_lscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      logical          ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists  ('pc_get_jdata')
      call cardset_get_scalar (obj%jdata, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,2)
      end subroutine pc_get_jdata_lscalar



      subroutine pc_get_jdata_cscalar (keyword,scalar)
      character(len=*) ,intent(in)     :: keyword           ! argument
      character(len=*) ,intent(out)    :: scalar            ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists  ('pc_get_jdata')
      call cardset_get_scalar (obj%jdata, keyword, scalar, errmsg)
      call pc_private_error   (errmsg,2)
      end subroutine pc_get_jdata_cscalar


!!------------------ private call trap and etrap --------------------------!!
!!------------------ private call trap and etrap --------------------------!!
!!------------------ private call trap and etrap --------------------------!!


      subroutine pc_private_call_trap (keyword, trap)
      character(len=*) ,intent(in)     :: keyword            ! argument
      external                            trap               ! argument
      optional                            trap               ! argument

      if (present(trap)) then
           if (pc_verify_scalar(keyword)) then
                call trap (keyword)
           end if
      end if
      end subroutine pc_private_call_trap



      subroutine pc_private_call_etrap (keyword, etrap)
      character(len=*) ,intent(in)     :: keyword            ! argument
      external                            etrap              ! argument
      optional                            etrap              ! argument
      integer                          :: indx               ! local
      integer                          :: action             ! local

      if (present(etrap)) then
           if (pc_verify_element(keyword,indx,action)) then
                call etrap (keyword,indx,action)
           end if
      end if
      end subroutine pc_private_call_etrap


!!--------------------------- alloc arrays -------------------------------!!
!!--------------------------- alloc arrays -------------------------------!!
!!--------------------------- alloc arrays -------------------------------!!


      subroutine pc_alloc_iarray (keyword,parray,nelements,etrap)
      character(len=*) ,intent(in)     :: keyword            ! argument
      integer          ,pointer        :: parray(:)          ! argument
      integer          ,intent(out)    :: nelements          ! argument
      external                            etrap              ! argument
      optional                            etrap              ! argument

      call pc_private_assert     ('pc_alloc')
      call pc_alloc_gui          (keyword,'ReplaceElements',parray,nelements)
      call pc_alloc_process      (keyword,                  parray,nelements)
      call pc_private_call_etrap (keyword,etrap)
      end subroutine pc_alloc_iarray



      subroutine pc_alloc_farray (keyword,parray,nelements,etrap)
      character(len=*) ,intent(in)     :: keyword            ! argument
      real             ,pointer        :: parray(:)          ! argument
      integer          ,intent(out)    :: nelements          ! argument
      external                            etrap              ! argument
      optional                            etrap              ! argument

      call pc_private_assert     ('pc_alloc')
      call pc_alloc_gui          (keyword,'ReplaceElements',parray,nelements)
      call pc_alloc_process      (keyword,                  parray,nelements)
      call pc_private_call_etrap (keyword,etrap)
      end subroutine pc_alloc_farray



      subroutine pc_alloc_darray (keyword,parray,nelements,etrap)
      character(len=*) ,intent(in)     :: keyword            ! argument
      double precision ,pointer        :: parray(:)          ! argument
      integer          ,intent(out)    :: nelements          ! argument
      external                            etrap              ! argument
      optional                            etrap              ! argument

      call pc_private_assert     ('pc_alloc')
      call pc_alloc_gui          (keyword,'ReplaceElements',parray,nelements)
      call pc_alloc_process      (keyword,                  parray,nelements)
      call pc_private_call_etrap (keyword,etrap)
      end subroutine pc_alloc_darray



      subroutine pc_alloc_carray (keyword,parray,nelements,etrap)
      character(len=*) ,intent(in)     :: keyword            ! argument
      character(len=*) ,pointer        :: parray(:)          ! argument
      integer          ,intent(out)    :: nelements          ! argument
      external                            etrap              ! argument
      optional                            etrap              ! argument

      call pc_private_assert     ('pc_alloc')
      call pc_alloc_gui          (keyword,'ReplaceElements',parray,nelements)
      call pc_alloc_process      (keyword,                  parray,nelements)
      call pc_private_call_etrap (keyword,etrap)
      end subroutine pc_alloc_carray



      subroutine pc_alloc_larray (keyword,parray,nelements,etrap)
      character(len=*) ,intent(in)     :: keyword            ! argument
      logical          ,pointer        :: parray(:)          ! argument
      integer          ,intent(out)    :: nelements          ! argument
      external                            etrap              ! argument
      optional                            etrap              ! argument

      call pc_private_assert     ('pc_alloc')
      call pc_alloc_gui          (keyword,'ReplaceElements',parray,nelements)
      call pc_alloc_process      (keyword,                  parray,nelements)
      call pc_private_call_etrap (keyword,etrap)
      end subroutine pc_alloc_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_alloc_process_iarray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      integer          ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_process')
      call cardset_alloc_array (obj%process, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_process_iarray



      subroutine pc_alloc_process_farray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      real             ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_process')
      call cardset_alloc_array (obj%process, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_process_farray



      subroutine pc_alloc_process_darray (keyword,array,nelements)
      character(len=*)        ,intent(in)    :: keyword            ! argument
      double precision        ,pointer       :: array(:)           ! argument
      integer                 ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_process')
      call cardset_alloc_array (obj%process, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_process_darray



      subroutine pc_alloc_process_larray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      logical          ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_process')
      call cardset_alloc_array (obj%process, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_process_larray



      subroutine pc_alloc_process_carray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_process')
      call cardset_alloc_array (obj%process, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_process_carray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_alloc_global_iarray (keyword,array,nelements)
      character(len=*) ,intent(in)     :: keyword            ! argument
      integer          ,pointer        :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      character(len=PC_LENGTH)         :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_global')
      call cardset_alloc_array (obj%global, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,3)
      if (errmsg /= ' ') nelements = 0      
      end subroutine pc_alloc_global_iarray



      subroutine pc_alloc_global_farray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      real             ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_global')
      call cardset_alloc_array (obj%global, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,3)
      if (errmsg /= ' ') nelements = 0      
      end subroutine pc_alloc_global_farray



      subroutine pc_alloc_global_darray (keyword,array,nelements)
      character(len=*)        ,intent(in)    :: keyword            ! argument
      double precision        ,pointer       :: array(:)           ! argument
      integer                 ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_global')
      call cardset_alloc_array (obj%global, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,3)
      if (errmsg /= ' ') nelements = 0      
      end subroutine pc_alloc_global_darray



      subroutine pc_alloc_global_larray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      logical          ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_global')
      call cardset_alloc_array (obj%global, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,3)
      if (errmsg /= ' ') nelements = 0      
      end subroutine pc_alloc_global_larray



      subroutine pc_alloc_global_carray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_global')
      call cardset_alloc_array (obj%global, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,3)
      if (errmsg /= ' ') nelements = 0      
      end subroutine pc_alloc_global_carray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_alloc_control_iarray (keyword,array,nelements)
      character(len=*) ,intent(in)     :: keyword            ! argument
      integer          ,pointer        :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      character(len=PC_LENGTH)         :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_control')
      call cardset_alloc_array (obj%control, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_control_iarray



      subroutine pc_alloc_control_farray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      real             ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_control')
      call cardset_alloc_array (obj%control, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_control_farray



      subroutine pc_alloc_control_darray (keyword,array,nelements)
      character(len=*)        ,intent(in)    :: keyword            ! argument
      double precision        ,pointer       :: array(:)           ! argument
      integer                 ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_control')
      call cardset_alloc_array (obj%control, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_control_darray



      subroutine pc_alloc_control_larray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      logical          ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_control')
      call cardset_alloc_array (obj%control, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_control_larray



      subroutine pc_alloc_control_carray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_control')
      call cardset_alloc_array (obj%control, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_control_carray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_alloc_gui_iarray (keyword,action,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,intent(in)    :: action             ! argument
      integer          ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: bigword            ! local
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_gui')
      call pc_private_combine  (keyword, action, bigword)
      call cardset_alloc_array (obj%gui, bigword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_gui_iarray



      subroutine pc_alloc_gui_farray (keyword,action,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,intent(in)    :: action             ! argument
      real             ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: bigword            ! local
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_gui')
      call pc_private_combine  (keyword, action, bigword)
      call cardset_alloc_array (obj%gui, bigword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_gui_farray



      subroutine pc_alloc_gui_darray (keyword,action,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,intent(in)    :: action             ! argument
      double precision ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: bigword            ! local
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_gui')
      call pc_private_combine  (keyword, action, bigword)
      call cardset_alloc_array (obj%gui, bigword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_gui_darray



      subroutine pc_alloc_gui_larray (keyword,action,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,intent(in)    :: action             ! argument
      logical          ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: bigword            ! local
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_gui')
      call pc_private_combine  (keyword, action, bigword)
      call cardset_alloc_array (obj%gui, bigword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_gui_larray



      subroutine pc_alloc_gui_carray (keyword,action,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,intent(in)    :: action             ! argument
      character(len=*) ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: bigword            ! local
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert   ('pc_alloc_gui')
      call pc_private_combine  (keyword, action, bigword)
      call cardset_alloc_array (obj%gui, bigword, array, nelements, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_alloc_gui_carray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_alloc_pdata_iarray (keyword,array,nelements)
      character(len=*) ,intent(in)     :: keyword            ! argument
      integer          ,pointer        :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      character(len=PC_LENGTH)         :: errmsg             ! local

      call pc_private_exists   ('pc_alloc_pdata')
      call cardset_alloc_array (obj%pdata, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,1)
      end subroutine pc_alloc_pdata_iarray



      subroutine pc_alloc_pdata_farray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      real             ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_exists   ('pc_alloc_pdata')
      call cardset_alloc_array (obj%pdata, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,1)
      end subroutine pc_alloc_pdata_farray



      subroutine pc_alloc_pdata_darray (keyword,array,nelements)
      character(len=*)        ,intent(in)    :: keyword            ! argument
      double precision        ,pointer       :: array(:)           ! argument
      integer                 ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_exists   ('pc_alloc_pdata')
      call cardset_alloc_array (obj%pdata, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,1)
      end subroutine pc_alloc_pdata_darray



      subroutine pc_alloc_pdata_larray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      logical          ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_exists   ('pc_alloc_pdata')
      call cardset_alloc_array (obj%pdata, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,1)
      end subroutine pc_alloc_pdata_larray



      subroutine pc_alloc_pdata_carray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_exists   ('pc_alloc_pdata')
      call cardset_alloc_array (obj%pdata, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,1)
      end subroutine pc_alloc_pdata_carray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_alloc_jdata_iarray (keyword,array,nelements)
      character(len=*) ,intent(in)     :: keyword            ! argument
      integer          ,pointer        :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      character(len=PC_LENGTH)         :: errmsg             ! local

      call pc_private_exists   ('pc_alloc_jdata')
      call cardset_alloc_array (obj%jdata, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,2)
      end subroutine pc_alloc_jdata_iarray



      subroutine pc_alloc_jdata_farray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      real             ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_exists   ('pc_alloc_jdata')
      call cardset_alloc_array (obj%jdata, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,2)
      end subroutine pc_alloc_jdata_farray



      subroutine pc_alloc_jdata_darray (keyword,array,nelements)
      character(len=*)        ,intent(in)    :: keyword            ! argument
      double precision        ,pointer       :: array(:)           ! argument
      integer                 ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_exists   ('pc_alloc_jdata')
      call cardset_alloc_array (obj%jdata, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,2)
      end subroutine pc_alloc_jdata_darray



      subroutine pc_alloc_jdata_larray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      logical          ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_exists   ('pc_alloc_jdata')
      call cardset_alloc_array (obj%jdata, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,2)
      end subroutine pc_alloc_jdata_larray



      subroutine pc_alloc_jdata_carray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,pointer       :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_exists   ('pc_alloc_jdata')
      call cardset_alloc_array (obj%jdata, keyword, array, nelements, errmsg)
      call pc_private_error    (errmsg,2)
      end subroutine pc_alloc_jdata_carray


!!----------------------- get arrays -----------------------------------!!
!!----------------------- get arrays -----------------------------------!!
!!----------------------- get arrays -----------------------------------!!


      subroutine pc_get_iarray (keyword,array,nelements,etrap)
      character(len=*) ,intent(in)     :: keyword            ! argument
      integer          ,intent(out)    :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      external                            etrap              ! argument
      optional                            etrap              ! argument

      call pc_private_assert     ('pc_get')
      call pc_get_gui            (keyword,'ReplaceElements',array,nelements)
      call pc_get_process        (keyword,                  array,nelements)
      call pc_private_call_etrap (keyword,etrap)
      end subroutine pc_get_iarray




      subroutine pc_get_farray (keyword,array,nelements,etrap)
      character(len=*) ,intent(in)     :: keyword            ! argument
      real             ,intent(out)    :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      external                            etrap              ! argument
      optional                            etrap              ! argument

      call pc_private_assert     ('pc_get')
      call pc_get_gui            (keyword,'ReplaceElements',array,nelements)
      call pc_get_process        (keyword,                  array,nelements)
      call pc_private_call_etrap (keyword,etrap)
      end subroutine pc_get_farray




      subroutine pc_get_darray (keyword,array,nelements,etrap)
      character(len=*) ,intent(in)     :: keyword            ! argument
      double precision ,intent(out)    :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      external                            etrap              ! argument
      optional                            etrap              ! argument

      call pc_private_assert     ('pc_get')
      call pc_get_gui            (keyword,'ReplaceElements',array,nelements)
      call pc_get_process        (keyword,                  array,nelements)
      call pc_private_call_etrap (keyword,etrap)
      end subroutine pc_get_darray




      subroutine pc_get_carray (keyword,array,nelements,etrap)
      character(len=*) ,intent(in)     :: keyword            ! argument
      character(len=*) ,intent(out)    :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      external                            etrap              ! argument
      optional                            etrap              ! argument

      call pc_private_assert     ('pc_get')
      call pc_get_gui            (keyword,'ReplaceElements',array,nelements)
      call pc_get_process        (keyword,                  array,nelements)
      call pc_private_call_etrap (keyword,etrap)
      end subroutine pc_get_carray




      subroutine pc_get_larray (keyword,array,nelements,etrap)
      character(len=*) ,intent(in)     :: keyword            ! argument
      logical          ,intent(out)    :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      external                            etrap              ! argument
      optional                            etrap              ! argument

      call pc_private_assert     ('pc_get')
      call pc_get_gui            (keyword,'ReplaceElements',array,nelements)
      call pc_get_process        (keyword,                  array,nelements)
      call pc_private_call_etrap (keyword,etrap)
      end subroutine pc_get_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_process_iarray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      integer          ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert ('pc_get_process')
      call cardset_get_array (obj%process, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg)
      end subroutine pc_get_process_iarray



      subroutine pc_get_process_farray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      real             ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert ('pc_get_process')
      call cardset_get_array (obj%process, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg)
      end subroutine pc_get_process_farray



      subroutine pc_get_process_darray (keyword,array,nelements)
      character(len=*)        ,intent(in)    :: keyword            ! argument
      double precision        ,intent(out)   :: array(:)           ! argument
      integer                 ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_assert ('pc_get_process')
      call cardset_get_array (obj%process, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg)
      end subroutine pc_get_process_darray



      subroutine pc_get_process_larray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      logical          ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert ('pc_get_process')
      call cardset_get_array (obj%process, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg)
      end subroutine pc_get_process_larray



      subroutine pc_get_process_carray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert ('pc_get_process')
      call cardset_get_array (obj%process, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg)
      end subroutine pc_get_process_carray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_global_iarray (keyword,array,nelements)
      character(len=*) ,intent(in)     :: keyword            ! argument
      integer          ,intent(out)    :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      character(len=PC_LENGTH)         :: errmsg             ! local

      call pc_private_assert ('pc_get_global')
      call cardset_get_array (obj%global, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,3)
      if (errmsg /= ' ') nelements = 0      
      end subroutine pc_get_global_iarray



      subroutine pc_get_global_farray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      real             ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert ('pc_get_global')
      call cardset_get_array (obj%global, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,3)
      if (errmsg /= ' ') nelements = 0      
      end subroutine pc_get_global_farray



      subroutine pc_get_global_darray (keyword,array,nelements)
      character(len=*)        ,intent(in)    :: keyword            ! argument
      double precision        ,intent(out)   :: array(:)           ! argument
      integer                 ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_assert ('pc_get_global')
      call cardset_get_array (obj%global, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,3)
      if (errmsg /= ' ') nelements = 0      
      end subroutine pc_get_global_darray



      subroutine pc_get_global_larray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      logical          ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert ('pc_get_global')
      call cardset_get_array (obj%global, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,3)
      if (errmsg /= ' ') nelements = 0      
      end subroutine pc_get_global_larray



      subroutine pc_get_global_carray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert ('pc_get_global')
      call cardset_get_array (obj%global, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,3)
      if (errmsg /= ' ') nelements = 0      
      end subroutine pc_get_global_carray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_control_iarray (keyword,array,nelements)
      character(len=*) ,intent(in)     :: keyword            ! argument
      integer          ,intent(out)    :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      character(len=PC_LENGTH)         :: errmsg             ! local

      call pc_private_assert ('pc_get_control')
      call cardset_get_array (obj%control, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg)
      end subroutine pc_get_control_iarray



      subroutine pc_get_control_farray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      real             ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert ('pc_get_control')
      call cardset_get_array (obj%control, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg)
      end subroutine pc_get_control_farray



      subroutine pc_get_control_darray (keyword,array,nelements)
      character(len=*)        ,intent(in)    :: keyword            ! argument
      double precision        ,intent(out)   :: array(:)           ! argument
      integer                 ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_assert ('pc_get_control')
      call cardset_get_array (obj%control, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg)
      end subroutine pc_get_control_darray



      subroutine pc_get_control_larray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      logical          ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert ('pc_get_control')
      call cardset_get_array (obj%control, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg)
      end subroutine pc_get_control_larray



      subroutine pc_get_control_carray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_assert ('pc_get_control')
      call cardset_get_array (obj%control, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg)
      end subroutine pc_get_control_carray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_gui_iarray (keyword,action,array,nelements)
      character(len=*) ,intent(in)           :: keyword            ! argument
      character(len=*) ,intent(in)           :: action             ! argument
      integer          ,intent(out)          :: array(:)           ! argument
      integer          ,intent(out)          :: nelements          ! argument
      character(len=PC_LENGTH)               :: bigword            ! local
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_assert  ('pc_get_gui')
      call pc_private_combine (keyword, action, bigword)
      call cardset_get_array  (obj%gui, bigword, array, nelements, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_gui_iarray



      subroutine pc_get_gui_farray (keyword,action,array,nelements)
      character(len=*) ,intent(in)           :: keyword            ! argument
      character(len=*) ,intent(in)           :: action             ! argument
      real             ,intent(out)          :: array(:)           ! argument
      integer          ,intent(out)          :: nelements          ! argument
      character(len=PC_LENGTH)               :: bigword            ! local
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_assert  ('pc_get_gui')
      call pc_private_combine (keyword, action, bigword)
      call cardset_get_array  (obj%gui, bigword, array, nelements, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_gui_farray



      subroutine pc_get_gui_darray (keyword,action,array,nelements)
      character(len=*) ,intent(in)           :: keyword            ! argument
      character(len=*) ,intent(in)           :: action             ! argument
      double precision ,intent(out)          :: array(:)           ! argument
      integer          ,intent(out)          :: nelements          ! argument
      character(len=PC_LENGTH)               :: bigword            ! local
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_assert  ('pc_get_gui')
      call pc_private_combine (keyword, action, bigword)
      call cardset_get_array  (obj%gui, bigword, array, nelements, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_gui_darray



      subroutine pc_get_gui_larray (keyword,action,array,nelements)
      character(len=*) ,intent(in)           :: keyword            ! argument
      character(len=*) ,intent(in)           :: action             ! argument
      logical          ,intent(out)          :: array(:)           ! argument
      integer          ,intent(out)          :: nelements          ! argument
      character(len=PC_LENGTH)               :: bigword            ! local
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_assert  ('pc_get_gui')
      call pc_private_combine (keyword, action, bigword)
      call cardset_get_array  (obj%gui, bigword, array, nelements, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_gui_larray



      subroutine pc_get_gui_carray (keyword,action,array,nelements)
      character(len=*) ,intent(in)           :: keyword            ! argument
      character(len=*) ,intent(in)           :: action             ! argument
      character(len=*) ,intent(out)          :: array(:)           ! argument
      integer          ,intent(out)          :: nelements          ! argument
      character(len=PC_LENGTH)               :: bigword            ! local
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_exists  ('pc_get_gui')
!!!   call pc_private_assert  ('pc_get_gui')
      call pc_private_combine (keyword, action, bigword)
      call cardset_get_array  (obj%gui, bigword, array, nelements, errmsg)
      call pc_private_error   (errmsg)
      end subroutine pc_get_gui_carray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_pdata_iarray (keyword,array,nelements)
      character(len=*) ,intent(in)     :: keyword            ! argument
      integer          ,intent(out)    :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      character(len=PC_LENGTH)         :: errmsg             ! local

      call pc_private_exists ('pc_get_pdata')
      call cardset_get_array (obj%pdata, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,1)
      end subroutine pc_get_pdata_iarray



      subroutine pc_get_pdata_farray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      real             ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_exists ('pc_get_pdata')
      call cardset_get_array (obj%pdata, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,1)
      end subroutine pc_get_pdata_farray



      subroutine pc_get_pdata_darray (keyword,array,nelements)
      character(len=*)        ,intent(in)    :: keyword            ! argument
      double precision        ,intent(out)   :: array(:)           ! argument
      integer                 ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_exists ('pc_get_pdata')
      call cardset_get_array (obj%pdata, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,1)
      end subroutine pc_get_pdata_darray



      subroutine pc_get_pdata_larray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      logical          ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_exists ('pc_get_pdata')
      call cardset_get_array (obj%pdata, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,1)
      end subroutine pc_get_pdata_larray



      subroutine pc_get_pdata_carray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_exists ('pc_get_pdata')
      call cardset_get_array (obj%pdata, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,1)
      end subroutine pc_get_pdata_carray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_jdata_iarray (keyword,array,nelements)
      character(len=*) ,intent(in)     :: keyword            ! argument
      integer          ,intent(out)    :: array(:)           ! argument
      integer          ,intent(out)    :: nelements          ! argument
      character(len=PC_LENGTH)         :: errmsg             ! local

      call pc_private_exists ('pc_get_jdata')
      call cardset_get_array (obj%jdata, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,2)
      end subroutine pc_get_jdata_iarray



      subroutine pc_get_jdata_farray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      real             ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_exists ('pc_get_jdata')
      call cardset_get_array (obj%jdata, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,2)
      end subroutine pc_get_jdata_farray



      subroutine pc_get_jdata_darray (keyword,array,nelements)
      character(len=*)        ,intent(in)    :: keyword            ! argument
      double precision        ,intent(out)   :: array(:)           ! argument
      integer                 ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)               :: errmsg             ! local

      call pc_private_exists ('pc_get_jdata')
      call cardset_get_array (obj%jdata, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,2)
      end subroutine pc_get_jdata_darray



      subroutine pc_get_jdata_larray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      logical          ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_exists ('pc_get_jdata')
      call cardset_get_array (obj%jdata, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,2)
      end subroutine pc_get_jdata_larray



      subroutine pc_get_jdata_carray (keyword,array,nelements)
      character(len=*) ,intent(in)    :: keyword            ! argument
      character(len=*) ,intent(out)   :: array(:)           ! argument
      integer          ,intent(out)   :: nelements          ! argument
      character(len=PC_LENGTH)        :: errmsg             ! local

      call pc_private_exists ('pc_get_jdata')
      call cardset_get_array (obj%jdata, keyword, array, nelements, errmsg)
      call pc_private_error  (errmsg,2)
      end subroutine pc_get_jdata_carray


!!------------------------- get array element -----------------------------!!
!!------------------------- get array element -----------------------------!!
!!------------------------- get array element -----------------------------!!


      subroutine pc_get_process_ielement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_process')
      call cardset_get_element (obj%process, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_process_ielement



      subroutine pc_get_process_felement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      real             ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_process')
      call cardset_get_element (obj%process, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_process_felement



      subroutine pc_get_process_delement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      double precision ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_process')
      call cardset_get_element (obj%process, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_process_delement



      subroutine pc_get_process_lelement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      logical          ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_process')
      call cardset_get_element (obj%process, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_process_lelement



      subroutine pc_get_process_celement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      character(len=*) ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_process')
      call cardset_get_element (obj%process, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_process_celement


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_global_ielement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_global')
      call cardset_get_element (obj%global, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,3)
      if (errmsg /= ' ') element = 100    
      end subroutine pc_get_global_ielement



      subroutine pc_get_global_felement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      real             ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_global')
      call cardset_get_element (obj%global, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,3)
      if (errmsg /= ' ') element = 0.0    
      end subroutine pc_get_global_felement



      subroutine pc_get_global_delement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      double precision ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_global')
      call cardset_get_element (obj%global, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,3)
      if (errmsg /= ' ') element = 0.0    
      end subroutine pc_get_global_delement



      subroutine pc_get_global_lelement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      logical          ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_global')
      call cardset_get_element (obj%global, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,3)
      if (errmsg /= ' ') element = .false.
      end subroutine pc_get_global_lelement



      subroutine pc_get_global_celement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      character(len=*) ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_global')
      call cardset_get_element (obj%global, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,3)
      if (errmsg /= ' ') element = ' '     
      end subroutine pc_get_global_celement


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_control_ielement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_control')
      call cardset_get_element (obj%control, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_control_ielement



      subroutine pc_get_control_felement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      real             ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_control')
      call cardset_get_element (obj%control, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_control_felement



      subroutine pc_get_control_delement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      double precision ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_control')
      call cardset_get_element (obj%control, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_control_delement



      subroutine pc_get_control_lelement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      logical          ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_control')
      call cardset_get_element (obj%control, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_control_lelement



      subroutine pc_get_control_celement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      character(len=*) ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_control')
      call cardset_get_element (obj%control, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_control_celement


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_gui_ielement (keyword,action,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      character(len=*) ,intent(in)     :: action            ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: bigword           ! local
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_gui')
      call pc_private_combine  (keyword, action,   bigword)
      call cardset_get_element (obj%gui, bigword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_gui_ielement



      subroutine pc_get_gui_felement (keyword,action,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      character(len=*) ,intent(in)     :: action            ! argument
      integer          ,intent(in)     :: indx              ! argument
      real             ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: bigword           ! local
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_gui')
      call pc_private_combine  (keyword, action,   bigword)
      call cardset_get_element (obj%gui, bigword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_gui_felement



      subroutine pc_get_gui_delement (keyword,action,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      character(len=*) ,intent(in)     :: action            ! argument
      integer          ,intent(in)     :: indx              ! argument
      double precision ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: bigword           ! local
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_gui')
      call pc_private_combine  (keyword, action,   bigword)
      call cardset_get_element (obj%gui, bigword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_gui_delement



      subroutine pc_get_gui_lelement (keyword,action,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      character(len=*) ,intent(in)     :: action            ! argument
      integer          ,intent(in)     :: indx              ! argument
      logical          ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: bigword           ! local
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_assert   ('pc_get_gui')
      call pc_private_combine  (keyword, action,   bigword)
      call cardset_get_element (obj%gui, bigword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_gui_lelement


      subroutine pc_get_gui_celement (keyword,action,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      character(len=*) ,intent(in)     :: action            ! argument
      integer          ,intent(in)     :: indx              ! argument
      character(len=*) ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: bigword           ! local
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists ('pc_get_gui')
!!!   call pc_private_assert   ('pc_get_gui')
      call pc_private_combine  (keyword, action,   bigword)
      call cardset_get_element (obj%gui, bigword, indx, element, errmsg)
      call pc_private_error    (errmsg)
      end subroutine pc_get_gui_celement


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_pdata_ielement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists   ('pc_get_pdata')
      call cardset_get_element (obj%pdata, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,1)
      end subroutine pc_get_pdata_ielement



      subroutine pc_get_pdata_felement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      real             ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists   ('pc_get_pdata')
      call cardset_get_element (obj%pdata, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,1)
      end subroutine pc_get_pdata_felement



      subroutine pc_get_pdata_delement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      double precision ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists   ('pc_get_pdata')
      call cardset_get_element (obj%pdata, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,1)
      end subroutine pc_get_pdata_delement



      subroutine pc_get_pdata_lelement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      logical          ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists   ('pc_get_pdata')
      call cardset_get_element (obj%pdata, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,1)
      end subroutine pc_get_pdata_lelement



      subroutine pc_get_pdata_celement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      character(len=*) ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists   ('pc_get_pdata')
      call cardset_get_element (obj%pdata, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,1)
      end subroutine pc_get_pdata_celement


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_get_jdata_ielement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      integer          ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists   ('pc_get_jdata')
      call cardset_get_element (obj%jdata, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,2)
      end subroutine pc_get_jdata_ielement



      subroutine pc_get_jdata_felement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      real             ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists   ('pc_get_jdata')
      call cardset_get_element (obj%jdata, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,2)
      end subroutine pc_get_jdata_felement



      subroutine pc_get_jdata_delement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      double precision ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists   ('pc_get_jdata')
      call cardset_get_element (obj%jdata, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,2)
      end subroutine pc_get_jdata_delement



      subroutine pc_get_jdata_lelement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      logical          ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists   ('pc_get_jdata')
      call cardset_get_element (obj%jdata, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,2)
      end subroutine pc_get_jdata_lelement



      subroutine pc_get_jdata_celement (keyword,indx,element)
      character(len=*) ,intent(in)     :: keyword           ! argument
      integer          ,intent(in)     :: indx              ! argument
      character(len=*) ,intent(out)    :: element           ! argument
      character(len=PC_LENGTH)         :: errmsg            ! local

      call pc_private_exists   ('pc_get_jdata')
      call cardset_get_element (obj%jdata, keyword, indx, element, errmsg)
      call pc_private_error    (errmsg,2)
      end subroutine pc_get_jdata_celement


!!---------------------------- call traps -------------------------------!!
!!---------------------------- call traps -------------------------------!!
!!---------------------------- call traps -------------------------------!!


      function pc_pressed    (keyword, ptrap) result (pressed)
      character(len=*) ,intent(in)     :: keyword           ! argument
      external                            ptrap             ! argument
      optional                            ptrap             ! argument
      logical                          :: pressed           ! result

      call pc_private_assert ('pc_pressed')
      !call pc_put_gui (keyword, 'THIS_IS_A_BUTTON', .true.) ! for SeisSpace.

      if (obj%update_state == PC_QUICK) then   ! later /= PC_GUI instead
           pressed = .false.
           return
      end if

      pressed = pc_gui_action_present(keyword, 'ButtonPress')
      if(pressed .and. present(ptrap)) then
             call ptrap(keyword)
      end if
      end function pc_pressed



      subroutine pc_call_array_trap    (keyword, atrap)
      character(len=*) ,intent(in)     :: keyword           ! argument
      external                            atrap             ! argument

      call pc_private_assert ('pc_call_array_trap')
      if (pc_verify_array(keyword)) call atrap (keyword)
      end subroutine pc_call_array_trap



      subroutine pc_call_arrayset_trap (keyword, astrap)
      character(len=*) ,intent(in)     :: keyword           ! argument
      external                            astrap            ! argument

      call pc_private_assert ('pc_call_arrayset_trap')
      if (pc_verify_arrayset(keyword)) call astrap (keyword)
      end subroutine pc_call_arrayset_trap



      subroutine pc_call_screen_trap   (keyword, strap)
      character(len=*) ,intent(in)     :: keyword           ! argument
      external                            strap             ! argument

      call pc_private_assert ('pc_call_screen_trap')
      if (pc_verify_screen(keyword)) call strap (keyword)
      end subroutine pc_call_screen_trap


      subroutine pc_call_end_trap   (endtrap)
      external                            endtrap           ! argument

      call pc_private_assert ('pc_call_end_trap')
      if (pc_verify_end()) call endtrap
      end subroutine pc_call_end_trap


!!-------------------------- activated --------------------------------!!
!!-------------------------- activated --------------------------------!!
!!-------------------------- activated --------------------------------!!


      function pc_activated () result (keyword)
      character(len=PC_LENGTH)      :: keyword           ! result
      character(len=PC_LENGTH)      :: smallword         ! local
      character(len=PC_LENGTH)      :: bigword           ! local
      character(len=PC_LENGTH)      :: keep              ! local
      character(len=PC_LENGTH)      :: action            ! local
      integer                       :: num,indx,kount    ! local

      call pc_private_assert ('pc_activated')
      keyword = ' '
      if (obj%update_state == PC_QUICK) return   ! later /= PC_GUI instead
      num = cardset_num_keywords(obj%gui)
      if (num == 0) return
      kount = 0
      keep = ' '
      do indx = 1,num
           bigword = cardset_get_keyword (obj%gui, indx)
           call pc_private_split         (bigword,   smallword, action)
           if (action == 'ModifyIndex' .or.  &
               action == 'ModifyField' .or.  &
               action == 'ButtonPress') then
                    kount = kount + 1
                    if (kount > 1) return
                    keep = smallword
           end if
      end do
      keyword = keep
      end function pc_activated


!!-------------------------- verify -----------------------------------!!
!!-------------------------- verify -----------------------------------!!
!!-------------------------- verify -----------------------------------!!


      function pc_verify_scalar (keyword) result (verify)
      character(len=*) ,intent(in) :: keyword       ! argument
      logical                      :: verify        ! result

      call pc_private_assert ('pc_verify_scalar')
      if (obj%update_state == PC_FRONTEND .or. &
          obj%update_state == PC_BACKEND  .or. &
          obj%update_state == PC_BACKEND_NO_EXEC) then
        verify = .true.
      else if (obj%update_state == PC_QUICK) then    ! later /= PC_GUI instead
        verify = .false.
      else if (pc_process_keyword_present(keyword)) then     ! later omit
        verify = .true.                                      ! later omit
      else
        verify = pc_gui_action_present(keyword, 'ModifyField')
      end if
      end function pc_verify_scalar



      function pc_verify_element (keyword,indx,action) result (verify)
      character(len=*) ,intent(in)  :: keyword       ! argument
      integer          ,intent(out) :: indx          ! argument
      integer          ,intent(out) :: action        ! argument
      logical                       :: verify        ! result
      character(len=PC_LENGTH)      :: smallword     ! local
      character(len=PC_LENGTH)      :: bigword       ! local
      character(len=PC_LENGTH)      :: errmsg        ! local

      call pc_private_assert ('pc_verify_element')
      if (obj%update_state == PC_QUICK) then   ! later /= PC_GUI instead
           verify = .false.
           indx   = 0
           action = PC_NOACTION
           return
      end if

      if (pc_gui_action_present(keyword, 'InsertIndex')) then
           smallword = 'InsertIndex'
           action    = PC_INSERT
      else if (pc_gui_action_present(keyword, 'RemoveIndex')) then
           smallword = 'RemoveIndex'
           action    = PC_REMOVE
      else if (pc_gui_action_present(keyword, 'ModifyIndex')) then
           smallword = 'ModifyIndex'
           action    = PC_MODIFY
      else
           verify = .false.
           indx   = 0
           action = PC_NOACTION
           return
      end if
      call pc_private_combine (keyword, smallword, bigword)
      call cardset_get_scalar (obj%gui, bigword, indx, errmsg)
      call pc_private_error   (errmsg)
      if (errmsg == ' ') then
           verify = .true.
      else
           verify = .false.
           indx   = 0
           action = PC_NOACTION
      end if
      end function pc_verify_element



      function pc_verify_array (keyword) result (verify)
      character(len=*) ,intent(in) :: keyword       ! argument
      logical                      :: verify        ! result

      call pc_private_assert ('pc_verify_array')
      if (obj%update_state == PC_FRONTEND .or. &
          obj%update_state == PC_BACKEND  .or. &
          obj%update_state == PC_BACKEND_NO_EXEC) then
        verify = .true.
      else if (obj%update_state == PC_QUICK) then    ! later /= PC_GUI instead
        verify = .false.
      else
        verify = pc_gui_action_present(keyword, 'LeaveArray')
      end if
      end function pc_verify_array



      function pc_verify_arrayset (keyword) result (verify)
      character(len=*) ,intent(in) :: keyword       ! argument
      logical                      :: verify        ! result

      call pc_private_assert ('pc_verify_arrayset')
      if (obj%update_state == PC_FRONTEND .or. &
          obj%update_state == PC_BACKEND  .or. &
          obj%update_state == PC_BACKEND_NO_EXEC) then
        verify = .true.
      else if (obj%update_state == PC_QUICK) then    ! later /= PC_GUI instead
        verify = .false.
      else
        verify = pc_gui_action_present(keyword, 'LeaveArrayset')
      end if
      end function pc_verify_arrayset



      function pc_verify_screen (keyword) result (verify)
      character(len=*) ,intent(in) :: keyword       ! argument
      logical                      :: verify        ! result

      call pc_private_assert ('pc_verify_screen')
      if (obj%update_state == PC_FRONTEND .or. &
          obj%update_state == PC_BACKEND  .or. &
          obj%update_state == PC_BACKEND_NO_EXEC) then
        verify = .true.
      else if (obj%update_state == PC_QUICK) then    ! later /= PC_GUI instead
        verify = .false.
      else
        verify = pc_gui_action_present(keyword, 'LeaveScreen')
      end if
      end function pc_verify_screen



      function pc_verify_end () result (verify)
      logical                      :: verify        ! result

      call pc_private_assert ('pc_verify_end')
      verify = (obj%update_state == PC_FRONTEND .or. &
                obj%update_state == PC_BACKEND  .or. &
                obj%update_state == PC_BACKEND_NO_EXEC)
      end function pc_verify_end


!!------------------------- put scalars -----------------------------------!!
!!------------------------- put scalars -----------------------------------!!
!!------------------------- put scalars -----------------------------------!!


      subroutine pc_put_gscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      type(grid_struct),intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert ('pc_put')
      call pc_put_process    (keyword,                scalar,nchar,ndec)
      call pc_put_gui        (keyword, 'ModifyField', scalar,nchar,ndec)
      end subroutine pc_put_gscalar

    ! later:
    ! if (obj%update_state == PC_FRONTEND .or. &              
    !     obj%update_state == PC_BACKEND  .or. &
    !     obj%update_state == PC_BACKEND_NO_EXEC) then
    !       call pc_put_process  (keyword, scalar,nchar,ndec)
    ! else if (obj%update_state == PC_GUI) then               
    !       call pc_put_gui      (keyword, 'ModifyField', scalar,nchar,ndec)
    ! end if                                                  

  ! alternative:
  !   character(len=PC_LENGTH)     :: bigword       ! local
  !   bigword = trim(keyword)//'#ModifyField'
  !   call cardset_put_scalar (obj%process, keyword, scalar,nchar,ndec)
  !   call cardset_put_scalar (obj%gui    , bigword, scalar,nchar,ndec)



      subroutine pc_put_iscalar (keyword,scalar,nchar)
      character(len=*) ,intent(in) :: keyword       ! argument
      integer          ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar         ! argument

      call pc_private_assert ('pc_put')
      call pc_put_process    (keyword,                scalar,nchar)
      call pc_put_gui        (keyword, 'ModifyField', scalar,nchar)
      end subroutine pc_put_iscalar



      subroutine pc_put_fscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      real             ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert ('pc_put')
      call pc_put_process    (keyword,                scalar,nchar,ndec)
      call pc_put_gui        (keyword, 'ModifyField', scalar,nchar,ndec)
      end subroutine pc_put_fscalar



      subroutine pc_put_dscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      double precision ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert ('pc_put')
      call pc_put_process    (keyword,                scalar,nchar,ndec)
      call pc_put_gui        (keyword, 'ModifyField', scalar,nchar,ndec)
      end subroutine pc_put_dscalar



      subroutine pc_put_cscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: scalar        ! argument

      call pc_private_assert ('pc_put')
      call pc_put_process    (keyword,                scalar)
      call pc_put_gui        (keyword, 'ModifyField', scalar)
      end subroutine pc_put_cscalar



      subroutine pc_put_lscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      logical          ,intent(in) :: scalar        ! argument

      call pc_private_assert    ('pc_put')
      call pc_put_options_field (keyword,(/.true.,.false./),2)
      call pc_put_process       (keyword,                scalar)
      call pc_put_gui           (keyword, 'ModifyField', scalar)
      end subroutine pc_put_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_process_gscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      type(grid_struct),intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_process')
      call cardset_put_scalar (obj%process, keyword, scalar,nchar,ndec)
      end subroutine pc_put_process_gscalar



      subroutine pc_put_process_iscalar (keyword,scalar,nchar)
      character(len=*) ,intent(in) :: keyword       ! argument
      integer          ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar         ! argument

      call pc_private_assert  ('pc_put_process')
      call cardset_put_scalar (obj%process, keyword, scalar,nchar)
      end subroutine pc_put_process_iscalar



      subroutine pc_put_process_fscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      real             ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_process')
      call cardset_put_scalar (obj%process, keyword, scalar,nchar,ndec)
      end subroutine pc_put_process_fscalar



      subroutine pc_put_process_dscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      double precision ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_process')
      call cardset_put_scalar (obj%process, keyword, scalar,nchar,ndec)
      end subroutine pc_put_process_dscalar



      subroutine pc_put_process_cscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: scalar        ! argument

      call pc_private_assert  ('pc_put_process')
      call cardset_put_scalar (obj%process, keyword, scalar)
      end subroutine pc_put_process_cscalar



      subroutine pc_put_process_lscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      logical          ,intent(in) :: scalar        ! argument

      call pc_private_assert  ('pc_put_process')
      call cardset_put_scalar (obj%process, keyword, scalar)
      end subroutine pc_put_process_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_global_gscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      type(grid_struct),intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_global')
      call cardset_put_scalar (obj%global, keyword, scalar,nchar,ndec)
      end subroutine pc_put_global_gscalar



      subroutine pc_put_global_iscalar (keyword,scalar,nchar)
      character(len=*) ,intent(in) :: keyword       ! argument
      integer          ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar         ! argument

      call pc_private_assert  ('pc_put_global')
      call cardset_put_scalar (obj%global, keyword, scalar,nchar)
      end subroutine pc_put_global_iscalar



      subroutine pc_put_global_fscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      real             ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_global')
      call cardset_put_scalar (obj%global, keyword, scalar,nchar,ndec)
      end subroutine pc_put_global_fscalar



      subroutine pc_put_global_dscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      double precision ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_global')
      call cardset_put_scalar (obj%global, keyword, scalar,nchar,ndec)
      end subroutine pc_put_global_dscalar



      subroutine pc_put_global_cscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: scalar        ! argument

      call pc_private_assert  ('pc_put_global')
      call cardset_put_scalar (obj%global, keyword, scalar)
      end subroutine pc_put_global_cscalar



      subroutine pc_put_global_lscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      logical          ,intent(in) :: scalar        ! argument

      call pc_private_assert  ('pc_put_global')
      call cardset_put_scalar (obj%global, keyword, scalar)
      end subroutine pc_put_global_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_control_gscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      type(grid_struct),intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_control')
      call cardset_put_scalar (obj%control, keyword, scalar,nchar,ndec)
      end subroutine pc_put_control_gscalar



      subroutine pc_put_control_iscalar (keyword,scalar,nchar)
      character(len=*) ,intent(in) :: keyword       ! argument
      integer          ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar         ! argument

      call pc_private_assert  ('pc_put_control')
      call cardset_put_scalar (obj%control, keyword, scalar,nchar)
      end subroutine pc_put_control_iscalar



      subroutine pc_put_control_fscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      real             ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_control')
      call cardset_put_scalar (obj%control, keyword, scalar,nchar,ndec)
      end subroutine pc_put_control_fscalar



      subroutine pc_put_control_dscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      double precision ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_control')
      call cardset_put_scalar (obj%control, keyword, scalar,nchar,ndec)
      end subroutine pc_put_control_dscalar



      subroutine pc_put_control_cscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: scalar        ! argument

      call pc_private_assert  ('pc_put_control')
      call cardset_put_scalar (obj%control, keyword, scalar)
      end subroutine pc_put_control_cscalar



      subroutine pc_put_control_lscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      logical          ,intent(in) :: scalar        ! argument

      call pc_private_assert  ('pc_put_control')
      call cardset_put_scalar (obj%control, keyword, scalar)
      end subroutine pc_put_control_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_gui_gscalar (keyword,action,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: action        ! argument
      type(grid_struct),intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_LENGTH)     :: bigword       ! local

      call pc_private_assert  ('pc_put_gui')
      call pc_private_combine (keyword, action,   bigword)
      call cardset_put_scalar (obj%gui, bigword, scalar,nchar,ndec)
      end subroutine pc_put_gui_gscalar



      subroutine pc_put_gui_iscalar (keyword,action,scalar,nchar)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: action        ! argument
      integer          ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar         ! argument
      character(len=PC_LENGTH)     :: bigword       ! local

      call pc_private_assert  ('pc_put_gui')
      call pc_private_combine (keyword, action,   bigword)
      call cardset_put_scalar (obj%gui, bigword, scalar,nchar)
      end subroutine pc_put_gui_iscalar



      subroutine pc_put_gui_fscalar (keyword,action,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: action        ! argument
      real             ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_LENGTH)     :: bigword       ! local

      call pc_private_assert  ('pc_put_gui')
      call pc_private_combine (keyword, action,   bigword)
      call cardset_put_scalar (obj%gui, bigword, scalar,nchar,ndec)
      end subroutine pc_put_gui_fscalar



      subroutine pc_put_gui_dscalar (keyword,action,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: action        ! argument
      double precision ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument
      character(len=PC_LENGTH)     :: bigword       ! local

      call pc_private_assert  ('pc_put_gui')
      call pc_private_combine (keyword, action,   bigword)
      call cardset_put_scalar (obj%gui, bigword, scalar,nchar,ndec)
      end subroutine pc_put_gui_dscalar



      subroutine pc_put_gui_cscalar (keyword,action,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: action        ! argument
      character(len=*) ,intent(in) :: scalar        ! argument
      character(len=PC_LENGTH)     :: bigword       ! local

      call pc_private_assert  ('pc_put_gui')
      call pc_private_combine (keyword, action,   bigword)
      call cardset_put_scalar (obj%gui, bigword, scalar)
      end subroutine pc_put_gui_cscalar



      subroutine pc_put_gui_lscalar (keyword,action,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: action        ! argument
      logical          ,intent(in) :: scalar        ! argument
      character(len=PC_LENGTH)     :: bigword       ! local

      call pc_private_assert  ('pc_put_gui')
      call pc_private_combine (keyword, action,   bigword)
      call cardset_put_scalar (obj%gui, bigword, scalar)
      end subroutine pc_put_gui_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_gui_only_gscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      type(grid_struct),intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert         ('pc_put_gui_only')
      call pc_remove_process_keyword (keyword)
      call pc_put_gui                (keyword, 'ModifyField', scalar,nchar,ndec)
      end subroutine pc_put_gui_only_gscalar



      subroutine pc_put_gui_only_iscalar (keyword,scalar,nchar)
      character(len=*) ,intent(in) :: keyword       ! argument
      integer          ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar         ! argument

      call pc_private_assert         ('pc_put_gui_only')
      call pc_remove_process_keyword (keyword)
      call pc_put_gui                (keyword, 'ModifyField', scalar,nchar)
      end subroutine pc_put_gui_only_iscalar



      subroutine pc_put_gui_only_fscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      real             ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert         ('pc_put_gui_only')
      call pc_remove_process_keyword (keyword)
      call pc_put_gui                (keyword, 'ModifyField', scalar,nchar,ndec)
      end subroutine pc_put_gui_only_fscalar



      subroutine pc_put_gui_only_dscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      double precision ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert         ('pc_put_gui_only')
      call pc_remove_process_keyword (keyword)
      call pc_put_gui                (keyword, 'ModifyField', scalar,nchar,ndec)
      end subroutine pc_put_gui_only_dscalar



      subroutine pc_put_gui_only_cscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: scalar        ! argument

      call pc_private_assert         ('pc_put_gui_only')
      call pc_remove_process_keyword (keyword)
      call pc_put_gui                (keyword, 'ModifyField', scalar)
      end subroutine pc_put_gui_only_cscalar



      subroutine pc_put_gui_only_lscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      logical          ,intent(in) :: scalar        ! argument

      call pc_private_assert         ('pc_put_gui_only')
      call pc_remove_process_keyword (keyword)
      call pc_put_gui                (keyword, 'ModifyField', scalar)
      end subroutine pc_put_gui_only_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_pdata_gscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      type(grid_struct),intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_pdata',1)
      call cardset_put_scalar (obj%pdata, keyword, scalar,nchar,ndec)
      end subroutine pc_put_pdata_gscalar



      subroutine pc_put_pdata_iscalar (keyword,scalar,nchar)
      character(len=*) ,intent(in) :: keyword       ! argument
      integer          ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar         ! argument

      call pc_private_assert  ('pc_put_pdata',1)
      call cardset_put_scalar (obj%pdata, keyword, scalar,nchar)
      end subroutine pc_put_pdata_iscalar



      subroutine pc_put_pdata_fscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      real             ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_pdata',1)
      call cardset_put_scalar (obj%pdata, keyword, scalar,nchar,ndec)
      end subroutine pc_put_pdata_fscalar



      subroutine pc_put_pdata_dscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      double precision ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_pdata',1)
      call cardset_put_scalar (obj%pdata, keyword, scalar,nchar,ndec)
      end subroutine pc_put_pdata_dscalar



      subroutine pc_put_pdata_cscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: scalar        ! argument

      call pc_private_assert  ('pc_put_pdata',1)
      call cardset_put_scalar (obj%pdata, keyword, scalar)
      end subroutine pc_put_pdata_cscalar



      subroutine pc_put_pdata_lscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      logical          ,intent(in) :: scalar        ! argument

      call pc_private_assert  ('pc_put_pdata',1)
      call cardset_put_scalar (obj%pdata, keyword, scalar)
      end subroutine pc_put_pdata_lscalar


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_jdata_gscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      type(grid_struct),intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_jdata',2)
      call cardset_put_scalar (obj%jdata, keyword, scalar,nchar,ndec)
      end subroutine pc_put_jdata_gscalar



      subroutine pc_put_jdata_iscalar (keyword,scalar,nchar)
      character(len=*) ,intent(in) :: keyword       ! argument
      integer          ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar         ! argument

      call pc_private_assert  ('pc_put_jdata',2)
      call cardset_put_scalar (obj%jdata, keyword, scalar,nchar)
      end subroutine pc_put_jdata_iscalar



      subroutine pc_put_jdata_fscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      real             ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_jdata',2)
      call cardset_put_scalar (obj%jdata, keyword, scalar,nchar,ndec)
      end subroutine pc_put_jdata_fscalar



      subroutine pc_put_jdata_dscalar (keyword,scalar,nchar,ndec)
      character(len=*) ,intent(in) :: keyword       ! argument
      double precision ,intent(in) :: scalar        ! argument
      integer,optional ,intent(in) :: nchar,ndec    ! argument

      call pc_private_assert  ('pc_put_jdata',2)
      call cardset_put_scalar (obj%jdata, keyword, scalar,nchar,ndec)
      end subroutine pc_put_jdata_dscalar



      subroutine pc_put_jdata_cscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      character(len=*) ,intent(in) :: scalar        ! argument

      call pc_private_assert  ('pc_put_jdata',2)
      call cardset_put_scalar (obj%jdata, keyword, scalar)
      end subroutine pc_put_jdata_cscalar



      subroutine pc_put_jdata_lscalar (keyword,scalar)
      character(len=*) ,intent(in) :: keyword       ! argument
      logical          ,intent(in) :: scalar        ! argument

      call pc_private_assert  ('pc_put_jdata',2)
      call cardset_put_scalar (obj%jdata, keyword, scalar)
      end subroutine pc_put_jdata_lscalar


!!------------------------- put arrays ---------------------------------!!
!!------------------------- put arrays ---------------------------------!!
!!------------------------- put arrays ---------------------------------!!


      subroutine pc_put_iarray (keyword,array,nelements,nchar)
      character(len=*) ,intent(in) :: keyword            ! argument
      integer          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar              ! argument

      call pc_private_assert ('pc_put')
      call pc_private_erase  (keyword)
      call pc_put_process    (keyword,                   array,nelements,nchar)
      call pc_put_gui        (keyword,'ReplaceElements', array,nelements,nchar)
      end subroutine pc_put_iarray


      subroutine pc_put_farray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      real             ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert ('pc_put')
      call pc_private_erase  (keyword)
      call pc_put_process (keyword,                  array,nelements,nchar,ndec)
      call pc_put_gui     (keyword,'ReplaceElements',array,nelements,nchar,ndec)
      end subroutine pc_put_farray


      subroutine pc_put_darray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      double precision ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert ('pc_put')
      call pc_private_erase  (keyword)
      call pc_put_process (keyword,                  array,nelements,nchar,ndec)
      call pc_put_gui     (keyword,'ReplaceElements',array,nelements,nchar,ndec)
      end subroutine pc_put_darray


      subroutine pc_put_carray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=*) ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      call pc_private_assert ('pc_put')
      call pc_private_erase  (keyword)
      call pc_put_process    (keyword,                    array,nelements)
      call pc_put_gui        (keyword, 'ReplaceElements', array,nelements)
      end subroutine pc_put_carray


      subroutine pc_put_larray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      logical          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert    ('pc_put')
      call pc_private_erase     (keyword)
      call pc_put_options_field (keyword,(/.true.,.false./),2)
      call pc_put_process       (keyword,                    array,nelements)
      call pc_put_gui           (keyword, 'ReplaceElements', array,nelements)
      end subroutine pc_put_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_process_iarray (keyword,array,nelements,nchar)
      character(len=*) ,intent(in) :: keyword            ! argument
      integer          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar              ! argument

      call pc_private_assert ('pc_put_process')
      call cardset_put_array (obj%process, keyword, array, nelements,nchar)
      end subroutine pc_put_process_iarray


      subroutine pc_put_process_farray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      real             ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert ('pc_put_process')
      call cardset_put_array (obj%process, keyword, array, nelements,nchar,ndec)
      end subroutine pc_put_process_farray


      subroutine pc_put_process_darray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      double precision ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert ('pc_put_process')
      call cardset_put_array (obj%process, keyword, array, nelements,nchar,ndec)
      end subroutine pc_put_process_darray


      subroutine pc_put_process_carray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=*) ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert ('pc_put_process')
      call cardset_put_array (obj%process, keyword, array, nelements)
      end subroutine pc_put_process_carray


      subroutine pc_put_process_larray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      logical          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert ('pc_put_process')
      call cardset_put_array (obj%process, keyword, array, nelements)
      end subroutine pc_put_process_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_global_iarray (keyword,array,nelements,nchar)
      character(len=*) ,intent(in) :: keyword            ! argument
      integer          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar              ! argument

      call pc_private_assert ('pc_put_global')
      call cardset_put_array (obj%global, keyword, array, nelements,nchar)
      end subroutine pc_put_global_iarray


      subroutine pc_put_global_farray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      real             ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert ('pc_put_global')
      call cardset_put_array (obj%global, keyword, array, nelements,nchar,ndec)
      end subroutine pc_put_global_farray


      subroutine pc_put_global_darray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      double precision ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert ('pc_put_global')
      call cardset_put_array (obj%global, keyword, array, nelements,nchar,ndec)
      end subroutine pc_put_global_darray


      subroutine pc_put_global_carray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=*) ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert ('pc_put_global')
      call cardset_put_array (obj%global, keyword, array, nelements)
      end subroutine pc_put_global_carray


      subroutine pc_put_global_larray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      logical          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert ('pc_put_global')
      call cardset_put_array (obj%global, keyword, array, nelements)
      end subroutine pc_put_global_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_control_iarray (keyword,array,nelements,nchar)
      character(len=*) ,intent(in) :: keyword            ! argument
      integer          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar              ! argument

      call pc_private_assert ('pc_put_control')
      call cardset_put_array (obj%control, keyword, array, nelements,nchar)
      end subroutine pc_put_control_iarray


      subroutine pc_put_control_farray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      real             ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert ('pc_put_control')
      call cardset_put_array (obj%control, keyword, array, nelements,nchar,ndec)
      end subroutine pc_put_control_farray


      subroutine pc_put_control_darray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      double precision ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert ('pc_put_control')
      call cardset_put_array (obj%control, keyword, array, nelements,nchar,ndec)
      end subroutine pc_put_control_darray


      subroutine pc_put_control_carray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=*) ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert ('pc_put_control')
      call cardset_put_array (obj%control, keyword, array, nelements)
      end subroutine pc_put_control_carray


      subroutine pc_put_control_larray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      logical          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert ('pc_put_control')
      call cardset_put_array (obj%control, keyword, array, nelements)
      end subroutine pc_put_control_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_gui_iarray (keyword,action,array,nelements,nchar)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=*) ,intent(in) :: action             ! argument
      integer          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar              ! argument
      character(len=PC_LENGTH)     :: bigword            ! local

      call pc_private_assert  ('pc_put_gui')
      call pc_private_combine (keyword, action,   bigword)
      call cardset_put_array  (obj%gui, bigword, array, nelements,nchar)
      end subroutine pc_put_gui_iarray


      subroutine pc_put_gui_farray (keyword,action,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=*) ,intent(in) :: action             ! argument
      real             ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument
      character(len=PC_LENGTH)     :: bigword            ! local

      call pc_private_assert  ('pc_put_gui')
      call pc_private_combine (keyword, action,   bigword)
      call cardset_put_array  (obj%gui, bigword, array, nelements,nchar,ndec)
      end subroutine pc_put_gui_farray


      subroutine pc_put_gui_darray (keyword,action,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=*) ,intent(in) :: action             ! argument
      double precision ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument
      character(len=PC_LENGTH)     :: bigword            ! local

      call pc_private_assert  ('pc_put_gui')
      call pc_private_combine (keyword, action,   bigword)
      call cardset_put_array  (obj%gui, bigword, array, nelements,nchar,ndec)
      end subroutine pc_put_gui_darray


      subroutine pc_put_gui_carray (keyword,action,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=*) ,intent(in) :: action             ! argument
      character(len=*) ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      character(len=PC_LENGTH)     :: bigword            ! local

      call pc_private_assert  ('pc_put_gui')
      call pc_private_combine (keyword, action,   bigword)
      call cardset_put_array  (obj%gui, bigword, array, nelements)
      end subroutine pc_put_gui_carray


      subroutine pc_put_gui_larray (keyword,action,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=*) ,intent(in) :: action             ! argument
      logical          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      character(len=PC_LENGTH)     :: bigword            ! local

      call pc_private_assert  ('pc_put_gui')
      call pc_private_combine (keyword, action,   bigword)
      call cardset_put_array  (obj%gui, bigword, array, nelements)
      end subroutine pc_put_gui_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!

! This routine erases old insert/remove/modify element actions which might
! be left over from the GUI:

      subroutine pc_private_erase (keyword)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=PC_LENGTH)     :: bigword            ! local

      call pc_private_combine     (keyword, 'InsertIndex', bigword)
      call cardset_remove_keyword (obj%gui, bigword)
      call pc_private_combine     (keyword, 'RemoveIndex', bigword)
      call cardset_remove_keyword (obj%gui, bigword)
      call pc_private_combine     (keyword, 'ModifyIndex', bigword)
      call cardset_remove_keyword (obj%gui, bigword)
      end subroutine pc_private_erase


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_gui_only_iarray (keyword,array,nelements,nchar)
      character(len=*) ,intent(in) :: keyword            ! argument
      integer          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar              ! argument

      call pc_private_assert         ('pc_put_gui_only')
      call pc_private_erase          (keyword)
      call pc_remove_process_keyword (keyword)
      call pc_put_gui (keyword, 'ReplaceElements', array, nelements,nchar)
      end subroutine pc_put_gui_only_iarray


      subroutine pc_put_gui_only_farray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      real             ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert         ('pc_put_gui_only')
      call pc_private_erase          (keyword)
      call pc_remove_process_keyword (keyword)
      call pc_put_gui (keyword, 'ReplaceElements', array, nelements,nchar,ndec)
      end subroutine pc_put_gui_only_farray


      subroutine pc_put_gui_only_darray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      double precision ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert         ('pc_put_gui_only')
      call pc_private_erase          (keyword)
      call pc_remove_process_keyword (keyword)
      call pc_put_gui (keyword, 'ReplaceElements', array, nelements,nchar,ndec)
      end subroutine pc_put_gui_only_darray


      subroutine pc_put_gui_only_carray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=*) ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert         ('pc_put_gui_only')
      call pc_private_erase          (keyword)
      call pc_remove_process_keyword (keyword)
      call pc_put_gui        (keyword, 'ReplaceElements', array, nelements)
      end subroutine pc_put_gui_only_carray


      subroutine pc_put_gui_only_larray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      logical          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert         ('pc_put_gui_only')
      call pc_private_erase          (keyword)
      call pc_remove_process_keyword (keyword)
      call pc_put_gui        (keyword, 'ReplaceElements', array, nelements)
      end subroutine pc_put_gui_only_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_pdata_iarray (keyword,array,nelements,nchar)
      character(len=*) ,intent(in) :: keyword            ! argument
      integer          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar              ! argument

      call pc_private_assert ('pc_put_pdata',1)
      call cardset_put_array (obj%pdata, keyword, array, nelements,nchar)
      end subroutine pc_put_pdata_iarray


      subroutine pc_put_pdata_farray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      real             ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert ('pc_put_pdata',1)
      call cardset_put_array (obj%pdata, keyword, array, nelements,nchar,ndec)
      end subroutine pc_put_pdata_farray


      subroutine pc_put_pdata_darray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      double precision ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert ('pc_put_pdata',1)
      call cardset_put_array (obj%pdata, keyword, array, nelements,nchar,ndec)
      end subroutine pc_put_pdata_darray


      subroutine pc_put_pdata_carray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=*) ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert ('pc_put_pdata',1)
      call cardset_put_array (obj%pdata, keyword, array, nelements)
      end subroutine pc_put_pdata_carray


      subroutine pc_put_pdata_larray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      logical          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert ('pc_put_pdata',1)
      call cardset_put_array (obj%pdata, keyword, array, nelements)
      end subroutine pc_put_pdata_larray


                          !!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_jdata_iarray (keyword,array,nelements,nchar)
      character(len=*) ,intent(in) :: keyword            ! argument
      integer          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar              ! argument

      call pc_private_assert ('pc_put_jdata',2)
      call cardset_put_array (obj%jdata, keyword, array, nelements,nchar)
      end subroutine pc_put_jdata_iarray


      subroutine pc_put_jdata_farray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      real             ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert ('pc_put_jdata',2)
      call cardset_put_array (obj%jdata, keyword, array, nelements,nchar,ndec)
      end subroutine pc_put_jdata_farray


      subroutine pc_put_jdata_darray (keyword,array,nelements,nchar,ndec)
      character(len=*) ,intent(in) :: keyword            ! argument
      double precision ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument
      integer,optional ,intent(in) :: nchar,ndec         ! argument

      call pc_private_assert ('pc_put_jdata',2)
      call cardset_put_array (obj%jdata, keyword, array, nelements,nchar,ndec)
      end subroutine pc_put_jdata_darray


      subroutine pc_put_jdata_carray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      character(len=*) ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert ('pc_put_jdata',2)
      call cardset_put_array (obj%jdata, keyword, array, nelements)
      end subroutine pc_put_jdata_carray


      subroutine pc_put_jdata_larray (keyword,array,nelements)
      character(len=*) ,intent(in) :: keyword            ! argument
      logical          ,intent(in) :: array(:)           ! argument
      integer          ,intent(in) :: nelements          ! argument

      call pc_private_assert ('pc_put_jdata',2)
      call cardset_put_array (obj%jdata, keyword, array, nelements)
      end subroutine pc_put_jdata_larray


!!----------------------- put GUI information -----------------------------!!
!!----------------------- put GUI information -----------------------------!!
!!----------------------- put GUI information -----------------------------!!

! This routine puts the list of array names into the gui parameters.
! Then this routine checks list of arrays to make sure they are all same length.
! The checking and fixing is done separately in the process and gui sections.
! General details:
!  (1) If all arrays are missing, nothing is done.
!  (2) If all arrays are present and the same length, nothing is done.
!  (3) Present arrays with unequal lengths generates an error message.
!  (4) Missing arrays do not generate an error message because they might
!       be informational arrays which are not present upon input, or only
!       a single array will be present if an element is modified (but not
!       inserted or removed) in the GUI.
! Process section details:
!  (1) All present arrays are truncated to the shortest array.
!  (2) All missing arrays are inserted with the length of the shortest array.
! Gui section details:
!  (1) All present arrays are removed if they are not equal lengths.
!  (2) All missing arrays remain missing.


      subroutine pc_register_array_names (keyword, arrays, narrays)
      character(len=*),intent(in)          :: keyword            ! argument
      character(len=*),intent(in)          :: arrays(:)          ! argument
      integer         ,intent(in),optional :: narrays            ! argument
      integer                              :: narrays2,indx      ! local
      integer                              :: length,lmin,lmax   ! local
      character(len=80)                    :: msg                ! local
      character(len=PC_LENGTH),pointer     :: arr(:)             ! local
      integer                              :: found              ! local


      nullify(arr)

      if (present(narrays)) then ; narrays2 = narrays
                            else ; narrays2 = size(arrays) ; end if

      call pc_private_assert ('pc_register_array_names')
      call pc_put_gui (keyword, 'ArrayNames', arrays, narrays2)
      call pc_put_process           (keyword, arrays, narrays2)

!!!!!!!!!! check the gui section:

      msg   = ' '
      lmin  = -1
      lmax  = -1
      do indx = 1,narrays2
           if (pc_gui_action_present(arrays(indx),'ReplaceElements')) then
                length = pc_num_elements_gui(arrays(indx),'ReplaceElements')
                msg    = trim(msg)//' '//arrays(indx)
                if (lmin == -1) then
                     lmin = length
                     lmax = length
                else
                     lmin = min(lmin,length)
                     lmax = max(lmax,length)
                end if
           end if
      end do

      if (lmin < lmax) then
           call pc_error ('these linked arrays have different lengths:')
           call pc_error (msg)
           call pc_error ('(the linked arrays have all been removed)')
           call pc_error ('(the linked arrays are GUI parameters)')
           do indx = 1,narrays2
                call pc_remove_gui_action (arrays(indx),'ReplaceElements')
                call pc_remove_gui_action (arrays(indx),'InsertIndex')
                call pc_remove_gui_action (arrays(indx),'RemoveIndex')
                call pc_remove_gui_action (arrays(indx),'ModifyIndex')
           end do
      end if

!!!!!!!!!! check the process section:

      found = 0
      msg   = ' '
      lmin  = -1
      lmax  = -1
      do indx = 1,narrays2
           if (pc_process_keyword_present(arrays(indx))) then
                found  = found + 1
                length = pc_num_elements_process(arrays(indx))
                msg    = trim(msg)//' '//arrays(indx)
                if (lmin == -1) then
                     lmin = length
                     lmax = length
                else
                     lmin = min(lmin,length)
                     lmax = max(lmax,length)
                end if
           end if
      end do

      if (lmin < lmax) then
           call pc_error ('these linked arrays have different lengths:')
           call pc_error (msg)
           call pc_error ('(the longer arrays have been truncated)')
           call pc_error ('(the linked arrays are PROCESS parameters)')
      end if

      if (lmin < lmax .or. (found > 0 .and. found < narrays2)) then
           allocate (arr(lmax+1))
           do indx = 1,narrays2
                arr(:) = ' '
                length = 0
                call pc_get_process (arrays(indx),arr,length)
                call pc_put_process (arrays(indx),arr,lmin)
           end do
           deallocate (arr)
      end if
      end subroutine pc_register_array_names


                        !!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_options_iscalar (keyword,options,noptions,nchar)
      character(len=*),intent(in) :: keyword              ! argument
      integer         ,intent(in) :: options(:)           ! argument
      integer,optional,intent(in) :: noptions             ! argument
      integer,optional,intent(in) :: nchar                ! argument
      integer                     :: noptions2            ! local

      if (present(noptions)) then ; noptions2 = noptions
                             else ; noptions2 = size(options) ; end if

      call pc_private_assert ('pc_put_options_field')
      call pc_put_gui (keyword, 'OptionsField', options, noptions2,nchar)
      end subroutine pc_put_options_iscalar


      subroutine pc_put_options_fscalar (keyword,options,noptions,nchar,ndec)
      character(len=*),intent(in) :: keyword              ! argument
      real            ,intent(in) :: options(:)           ! argument
      integer,optional,intent(in) :: noptions             ! argument
      integer,optional,intent(in) :: nchar,ndec           ! argument
      integer                     :: noptions2            ! local

      if (present(noptions)) then ; noptions2 = noptions
                             else ; noptions2 = size(options) ; end if

      call pc_private_assert ('pc_put_options_field')
      call pc_put_gui (keyword, 'OptionsField', options, noptions2,nchar,ndec)
      end subroutine pc_put_options_fscalar


      subroutine pc_put_options_dscalar (keyword,options,noptions,nchar,ndec)
      character(len=*),intent(in) :: keyword              ! argument
      double precision,intent(in) :: options(:)           ! argument
      integer,optional,intent(in) :: noptions             ! argument
      integer,optional,intent(in) :: nchar,ndec           ! argument
      integer                     :: noptions2            ! local

      if (present(noptions)) then ; noptions2 = noptions
                             else ; noptions2 = size(options) ; end if

      call pc_private_assert ('pc_put_options_field')
      call pc_put_gui (keyword, 'OptionsField', options, noptions2,nchar,ndec)
      end subroutine pc_put_options_dscalar


      subroutine pc_put_options_cscalar (keyword,options,noptions)
      character(len=*),intent(in) :: keyword              ! argument
      character(len=*),intent(in) :: options(:)           ! argument
      integer,optional,intent(in) :: noptions             ! argument
      integer                     :: noptions2            ! local

      if (present(noptions)) then ; noptions2 = noptions
                             else ; noptions2 = size(options) ; end if

      call pc_private_assert ('pc_put_options_field')
      call pc_put_gui        (keyword, 'OptionsField', options, noptions2)
      end subroutine pc_put_options_cscalar


      subroutine pc_put_options_lscalar (keyword,options,noptions)
      character(len=*),intent(in) :: keyword              ! argument
      logical         ,intent(in) :: options(:)           ! argument
      integer,optional,intent(in) :: noptions             ! argument
      integer                     :: noptions2            ! local

      if (present(noptions)) then ; noptions2 = noptions
                             else ; noptions2 = size(options) ; end if

      call pc_private_assert ('pc_put_options_field')
      call pc_put_gui        (keyword, 'OptionsField', options, noptions2)
      end subroutine pc_put_options_lscalar

                        !!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine pc_put_options_iarray (keyword,options,noptions,nchar)
      character(len=*),intent(in) :: keyword              ! argument
      integer         ,intent(in) :: options(:)           ! argument
      integer,optional,intent(in) :: noptions             ! argument
      integer,optional,intent(in) :: nchar                ! argument
      integer                     :: noptions2            ! local

      if (present(noptions)) then ; noptions2 = noptions
                             else ; noptions2 = size(options) ; end if

      call pc_private_assert ('pc_put_options_array')
      call pc_put_gui (keyword, 'OptionsArray', options, noptions2,nchar)
      end subroutine pc_put_options_iarray


      subroutine pc_put_options_farray (keyword,options,noptions,nchar,ndec)
      character(len=*),intent(in) :: keyword              ! argument
      real            ,intent(in) :: options(:)           ! argument
      integer,optional,intent(in) :: noptions             ! argument
      integer,optional,intent(in) :: nchar,ndec           ! argument
      integer                     :: noptions2            ! local

      if (present(noptions)) then ; noptions2 = noptions
                             else ; noptions2 = size(options) ; end if

      call pc_private_assert ('pc_put_options_array')
      call pc_put_gui (keyword, 'OptionsArray', options, noptions2,nchar,ndec)
      end subroutine pc_put_options_farray


      subroutine pc_put_options_darray (keyword,options,noptions,nchar,ndec)
      character(len=*),intent(in) :: keyword              ! argument
      double precision,intent(in) :: options(:)           ! argument
      integer,optional,intent(in) :: noptions             ! argument
      integer,optional,intent(in) :: nchar,ndec           ! argument
      integer                     :: noptions2            ! local

      if (present(noptions)) then ; noptions2 = noptions
                             else ; noptions2 = size(options) ; end if

      call pc_private_assert ('pc_put_options_array')
      call pc_put_gui (keyword, 'OptionsArray', options, noptions2,nchar,ndec)
      end subroutine pc_put_options_darray


      subroutine pc_put_options_carray (keyword,options,noptions)
      character(len=*),intent(in) :: keyword              ! argument
      character(len=*),intent(in) :: options(:)           ! argument
      integer,optional,intent(in) :: noptions             ! argument
      integer                     :: noptions2            ! local

      if (present(noptions)) then ; noptions2 = noptions
                             else ; noptions2 = size(options) ; end if

      call pc_private_assert ('pc_put_options_array')
      call pc_put_gui        (keyword, 'OptionsArray', options, noptions2)
      end subroutine pc_put_options_carray


      subroutine pc_put_options_larray (keyword,options,noptions)
      character(len=*),intent(in) :: keyword              ! argument
      logical         ,intent(in) :: options(:)           ! argument
      integer,optional,intent(in) :: noptions             ! argument
      integer                     :: noptions2            ! local

      if (present(noptions)) then ; noptions2 = noptions
                             else ; noptions2 = size(options) ; end if

      call pc_private_assert ('pc_put_options_array')
      call pc_put_gui        (keyword, 'OptionsArray', options, noptions2)
      end subroutine pc_put_options_larray


                        !!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine pc_put_sensitive_field_flag (keyword,sensitive)
      character(len=*),intent(in)  :: keyword       ! argument
      logical         ,intent(in)  :: sensitive     ! argument

      call pc_private_assert ('pc_put_sensitive_field_flag')
      call pc_put_gui        (keyword, 'SensitiveField', sensitive)
      end subroutine pc_put_sensitive_field_flag


      subroutine pc_put_sensitive_array_flag (keyword,sensitive)
      character(len=*),intent(in)  :: keyword       ! argument
      logical         ,intent(in)  :: sensitive     ! argument

      call pc_private_assert ('pc_put_sensitive_array_flag')
      call pc_put_gui        (keyword, 'SensitiveArray', sensitive)
      end subroutine pc_put_sensitive_array_flag


      subroutine pc_put_sensitive_arrayset_flag (keyword,sensitive)
      character(len=*),intent(in)  :: keyword       ! argument
      logical         ,intent(in)  :: sensitive     ! argument

      call pc_private_assert ('pc_put_sensitive_arrayset_flag')
      call pc_put_gui        (keyword, 'SensitiveArrayset', sensitive)
      end subroutine pc_put_sensitive_arrayset_flag


      subroutine pc_put_sensitive_screen_flag (keyword,sensitive)
      character(len=*),intent(in)  :: keyword       ! argument
      logical         ,intent(in)  :: sensitive     ! argument

      call pc_private_assert ('pc_put_sensitive_screen_flag')
      call pc_put_gui        (keyword, 'SensitiveScreen', sensitive)
      end subroutine pc_put_sensitive_screen_flag

                        !!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine pc_put_visible_flag (keyword,visible)
      character(len=*),intent(in)  :: keyword       ! argument
      logical         ,intent(in)  :: visible       ! argument

      call pc_private_assert ('pc_put_visible_flag')
      call pc_put_gui        (          keyword, 'Visible', visible)
      call pc_put_gui        ('LABEL_'//keyword, 'Visible', visible)
      end subroutine pc_put_visible_flag

                        !!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine pc_put_minsize_array (keyword,minsize)
      character(len=*),intent(in)  :: keyword       ! argument
      integer         ,intent(in)  :: minsize       ! argument

      call pc_private_assert ('pc_put_minsize_array')
      call pc_put_gui        (keyword, 'MinsizeArray', minsize)
      end subroutine pc_put_minsize_array


      subroutine pc_put_minsize_arrayset (keyword,minsize)
      character(len=*),intent(in)  :: keyword       ! argument
      integer         ,intent(in)  :: minsize       ! argument

      call pc_private_assert ('pc_put_minsize_arrayset')
      call pc_put_gui        (keyword, 'MinsizeArrayset', minsize)
      end subroutine pc_put_minsize_arrayset


      subroutine pc_put_maxsize_array (keyword,maxsize)
      character(len=*),intent(in)  :: keyword       ! argument
      integer         ,intent(in)  :: maxsize       ! argument

      call pc_private_assert ('pc_put_maxsize_array')
      call pc_put_gui        (keyword, 'MaxsizeArray', maxsize)
      end subroutine pc_put_maxsize_array


      subroutine pc_put_maxsize_arrayset (keyword,maxsize)
      character(len=*),intent(in)  :: keyword       ! argument
      integer         ,intent(in)  :: maxsize       ! argument

      call pc_private_assert ('pc_put_maxsize_arrayset')
      call pc_put_gui        (keyword, 'MaxsizeArrayset', maxsize)
      end subroutine pc_put_maxsize_arrayset

                        !!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine pc_jump_screen (keyword)
      character(len=*),intent(in)  :: keyword       ! argument

      call pc_private_assert ('pc_jump_screen')
      call pc_put_gui        (keyword, 'JumpScreen', ' ')
      end subroutine pc_jump_screen


      subroutine pc_jump_field (keyword)
      character(len=*),intent(in)  :: keyword       ! argument

      call pc_private_assert ('pc_jump_field')
      call pc_put_gui        (keyword, 'JumpField', ' ')
      end subroutine pc_jump_field


      subroutine pc_jump_arrayset (keyword)
      character(len=*),intent(in)  :: keyword       ! argument

      call pc_private_assert ('pc_jump_arrayset')
      call pc_put_gui        (keyword, 'JumpArrayset', ' ')
      end subroutine pc_jump_arrayset


      subroutine pc_jump_arrayset_row (keyword, indx)
      character(len=*),intent(in)  :: keyword       ! argument
      integer         ,intent(in)  :: indx          ! argument

      call pc_private_assert ('pc_jump_arrayset_row')
      call pc_put_gui        (keyword, 'JumpArraysetRow', indx)
      end subroutine pc_jump_arrayset_row


      subroutine pc_jump_array_element (keyword, indx)
      character(len=*),intent(in)  :: keyword       ! argument
      integer         ,intent(in)  :: indx          ! argument

      call pc_private_assert ('pc_jump_array_element')
      call pc_put_gui        (keyword, 'JumpArrayElement', indx)
      end subroutine pc_jump_array_element


      subroutine pc_jump_array (keyword)
      character(len=*),intent(in)  :: keyword       ! argument

      call pc_private_assert ('pc_jump_array')
      call pc_put_gui        (keyword, 'JumpArray', ' ')
      end subroutine pc_jump_array


!!------------------------ get and put data cards -------------------------!!
!!------------------------ get and put data cards -------------------------!!
!!------------------------ get and put data cards -------------------------!!


      function pc_num_process_cards () result (ncards)
      integer                      :: ncards         ! result

      call pc_private_assert     ('pc_num_process_cards')
      ncards = cardset_num_cards (obj%process)
      end function pc_num_process_cards


      subroutine pc_alloc_process_cards (pcards,ncards)
      character(len=*),pointer     :: pcards(:)      ! argument
      integer         ,intent(out) :: ncards         ! argument

      call pc_private_assert   ('pc_alloc_process_cards')
      call cardset_alloc_cards (obj%process, pcards, ncards)
      end subroutine pc_alloc_process_cards


      subroutine pc_get_process_cards (cards,ncards,errmsg)
      character(len=*),intent(out) :: cards(:)       ! argument
      integer         ,intent(out) :: ncards         ! argument
      character(len=*),intent(out) :: errmsg         ! argument

      call pc_private_assert ('pc_get_process_cards')
      call cardset_get_cards (obj%process, cards, ncards, errmsg)
      end subroutine pc_get_process_cards


      subroutine pc_get_process_card (icard,card,errmsg)
      integer         ,intent(in)  :: icard          ! argument
      character(len=*),intent(out) :: card           ! argument
      character(len=*),intent(out) :: errmsg         ! argument

      call pc_private_assert ('pc_get_process_card')
      call cardset_get_card  (obj%process, icard, card, errmsg)
      end subroutine pc_get_process_card


      subroutine pc_put_process_cards (cards,ncards)
      character(len=*),intent(in) :: cards(:)       ! argument
      integer         ,intent(in) :: ncards         ! argument

      call pc_private_assert ('pc_put_process_cards')
      call cardset_put_cards (obj%process, cards, ncards)
      end subroutine pc_put_process_cards


      subroutine pc_put_process_card (card)
      character(len=*),intent(in) :: card                ! argument

      call pc_private_assert ('pc_put_process_card')
      call cardset_put_card  (obj%process, card)
      end subroutine pc_put_process_card


      subroutine pc_add_process_card (card)
      character(len=*),intent(in) :: card                ! argument

      call pc_private_assert ('pc_add_process_card')
      call cardset_add_card  (obj%process, card)
      end subroutine pc_add_process_card


      subroutine pc_clear_process_cards

      call pc_private_assert ('pc_clear_process_cards')
      call cardset_clear     (obj%process)
      end subroutine pc_clear_process_cards


                         !!!!!!!!!!!!!!!!!!!!!!!


      function pc_num_global_cards () result (ncards)
      integer                      :: ncards         ! result

      call pc_private_assert     ('pc_num_global_cards')
      ncards = cardset_num_cards (obj%global)
      end function pc_num_global_cards


      subroutine pc_alloc_global_cards (pcards,ncards)
      character(len=*),pointer     :: pcards(:)      ! argument
      integer         ,intent(out) :: ncards         ! argument

      call pc_private_assert   ('pc_alloc_global_cards')
      call cardset_alloc_cards (obj%global, pcards, ncards)
      end subroutine pc_alloc_global_cards


      subroutine pc_get_global_cards (cards,ncards,errmsg)
      character(len=*),intent(out) :: cards(:)       ! argument
      integer         ,intent(out) :: ncards         ! argument
      character(len=*),intent(out) :: errmsg         ! argument

      call pc_private_assert ('pc_get_global_cards')
      call cardset_get_cards (obj%global, cards, ncards, errmsg)
      end subroutine pc_get_global_cards


      subroutine pc_get_global_card (icard,card,errmsg)
      integer         ,intent(in)  :: icard          ! argument
      character(len=*),intent(out) :: card           ! argument
      character(len=*),intent(out) :: errmsg         ! argument

      call pc_private_assert ('pc_get_global_card')
      call cardset_get_card  (obj%global, icard, card, errmsg)
      end subroutine pc_get_global_card


      subroutine pc_put_global_cards (cards,ncards)
      character(len=*),intent(in)  :: cards(:)       ! argument
      integer         ,intent(in)  :: ncards         ! argument

      call pc_private_assert ('pc_put_global_cards')
      call cardset_put_cards (obj%global, cards, ncards)
      end subroutine pc_put_global_cards


      subroutine pc_put_global_card (card)
      character(len=*),intent(in) :: card                ! argument

      call pc_private_assert ('pc_put_global_card')
      call cardset_put_card  (obj%global, card)
      end subroutine pc_put_global_card


      subroutine pc_add_global_card (card)
      character(len=*),intent(in) :: card                ! argument

      call pc_private_assert ('pc_add_global_card')
      call cardset_add_card  (obj%global, card)
      end subroutine pc_add_global_card


      subroutine pc_clear_global_cards

      call pc_private_assert ('pc_clear_global_cards')
      call cardset_clear     (obj%global)
      end subroutine pc_clear_global_cards


                         !!!!!!!!!!!!!!!!!!!!!!!


      function pc_num_control_cards () result (ncards)
      integer                      :: ncards         ! result

      call pc_private_assert     ('pc_num_control_cards')
      ncards = cardset_num_cards (obj%control)
      end function pc_num_control_cards


      subroutine pc_alloc_control_cards (pcards,ncards)
      character(len=*),pointer     :: pcards(:)      ! argument
      integer         ,intent(out) :: ncards         ! argument

      call pc_private_assert   ('pc_alloc_control_cards')
      call cardset_alloc_cards (obj%control, pcards, ncards)
      end subroutine pc_alloc_control_cards


      subroutine pc_get_control_cards (cards,ncards,errmsg)
      character(len=*),intent(out) :: cards(:)       ! argument
      integer         ,intent(out) :: ncards         ! argument
      character(len=*),intent(out) :: errmsg         ! argument

      call pc_private_assert ('pc_get_control_cards')
      call cardset_get_cards (obj%control, cards, ncards, errmsg)
      end subroutine pc_get_control_cards


      subroutine pc_get_control_card (icard,card,errmsg)
      integer         ,intent(in)  :: icard          ! argument
      character(len=*),intent(out) :: card           ! argument
      character(len=*),intent(out) :: errmsg         ! argument

      call pc_private_assert ('pc_get_control_card')
      call cardset_get_card  (obj%control, icard, card, errmsg)
      end subroutine pc_get_control_card


      subroutine pc_put_control_cards (cards,ncards)
      character(len=*),intent(in)  :: cards(:)       ! argument
      integer         ,intent(in)  :: ncards         ! argument

      call pc_private_assert ('pc_put_control_cards')
      call cardset_put_cards (obj%control, cards, ncards)
      end subroutine pc_put_control_cards


      subroutine pc_put_control_card (card)
      character(len=*),intent(in) :: card                ! argument

      call pc_private_assert ('pc_put_control_card')
      call cardset_put_card  (obj%control, card)
      end subroutine pc_put_control_card


      subroutine pc_add_control_card (card)
      character(len=*),intent(in) :: card                ! argument

      call pc_private_assert ('pc_add_control_card')
      call cardset_add_card  (obj%control, card)
      end subroutine pc_add_control_card


      subroutine pc_clear_control_cards

      call pc_private_assert ('pc_clear_control_cards')
      call cardset_clear     (obj%control)
      end subroutine pc_clear_control_cards


                         !!!!!!!!!!!!!!!!!!!!!!!


      function pc_num_pdata_cards () result (ncards)
      integer                      :: ncards         ! result

      call pc_private_exists     ('pc_num_pdata_cards')
      ncards = cardset_num_cards (obj%pdata)
      end function pc_num_pdata_cards


      subroutine pc_alloc_pdata_cards (pcards,ncards)
      character(len=*),pointer     :: pcards(:)      ! argument
      integer         ,intent(out) :: ncards         ! argument

      call pc_private_exists   ('pc_alloc_pdata_cards')
      call cardset_alloc_cards (obj%pdata, pcards, ncards)
      end subroutine pc_alloc_pdata_cards


      subroutine pc_get_pdata_cards (cards,ncards,errmsg)
      character(len=*),intent(out) :: cards(:)       ! argument
      integer         ,intent(out) :: ncards         ! argument
      character(len=*),intent(out) :: errmsg         ! argument

      call pc_private_exists ('pc_get_pdata_cards')
      call cardset_get_cards (obj%pdata, cards, ncards, errmsg)
      end subroutine pc_get_pdata_cards


      subroutine pc_get_pdata_card (icard,card,errmsg)
      integer         ,intent(in)  :: icard          ! argument
      character(len=*),intent(out) :: card           ! argument
      character(len=*),intent(out) :: errmsg         ! argument

      call pc_private_exists ('pc_get_pdata_card')
      call cardset_get_card  (obj%pdata, icard, card, errmsg)
      end subroutine pc_get_pdata_card


      subroutine pc_put_pdata_cards (cards,ncards)
      character(len=*),intent(in)  :: cards(:)       ! argument
      integer         ,intent(in)  :: ncards         ! argument

      call pc_private_assert ('pc_put_pdata_cards',1)
      call cardset_put_cards (obj%pdata, cards, ncards)
      end subroutine pc_put_pdata_cards


      subroutine pc_put_pdata_card (card)
      character(len=*),intent(in) :: card                ! argument

      call pc_private_assert ('pc_put_pdata_card',1)
      call cardset_put_card  (obj%pdata, card)
      end subroutine pc_put_pdata_card


      subroutine pc_add_pdata_card (card)
      character(len=*),intent(in) :: card                ! argument

      call pc_private_assert ('pc_add_pdata_card',1)
      call cardset_add_card  (obj%pdata, card)
      end subroutine pc_add_pdata_card


      subroutine pc_clear_pdata_cards

      call pc_private_assert ('pc_clear_pdata_cards',1)
      call cardset_clear     (obj%pdata)
      end subroutine pc_clear_pdata_cards


                         !!!!!!!!!!!!!!!!!!!!!!!


      function pc_num_jdata_cards () result (ncards)
      integer                      :: ncards         ! result

      call pc_private_exists     ('pc_num_jdata_cards')
      ncards = cardset_num_cards (obj%jdata)
      end function pc_num_jdata_cards


      subroutine pc_alloc_jdata_cards (pcards,ncards)
      character(len=*),pointer     :: pcards(:)      ! argument
      integer         ,intent(out) :: ncards         ! argument

      call pc_private_exists   ('pc_alloc_jdata_cards')
      call cardset_alloc_cards (obj%jdata, pcards, ncards)
      end subroutine pc_alloc_jdata_cards


      subroutine pc_get_jdata_cards (cards,ncards,errmsg)
      character(len=*),intent(out) :: cards(:)       ! argument
      integer         ,intent(out) :: ncards         ! argument
      character(len=*),intent(out) :: errmsg         ! argument

      call pc_private_exists ('pc_get_jdata_cards')
      call cardset_get_cards (obj%jdata, cards, ncards, errmsg)
      end subroutine pc_get_jdata_cards


      subroutine pc_get_jdata_card (icard,card,errmsg)
      integer         ,intent(in)  :: icard          ! argument
      character(len=*),intent(out) :: card           ! argument
      character(len=*),intent(out) :: errmsg         ! argument

      call pc_private_exists ('pc_get_jdata_card')
      call cardset_get_card  (obj%jdata, icard, card, errmsg)
      end subroutine pc_get_jdata_card


      subroutine pc_put_jdata_cards (cards,ncards)
      character(len=*),intent(in)  :: cards(:)       ! argument
      integer         ,intent(in)  :: ncards         ! argument

      call pc_private_assert ('pc_put_jdata_cards',2)
      call cardset_put_cards (obj%jdata, cards, ncards)
      end subroutine pc_put_jdata_cards


      subroutine pc_put_jdata_card (card)
      character(len=*),intent(in) :: card                ! argument

      call pc_private_assert ('pc_put_jdata_card',2)
      call cardset_put_card  (obj%jdata, card)
      end subroutine pc_put_jdata_card


      subroutine pc_add_jdata_card (card)
      character(len=*),intent(in) :: card                ! argument

      call pc_private_assert ('pc_add_jdata_card',2)
      call cardset_add_card  (obj%jdata, card)
      end subroutine pc_add_jdata_card


      subroutine pc_clear_jdata_cards

      call pc_private_assert ('pc_clear_jdata_cards',2)
      call cardset_clear     (obj%jdata)
      end subroutine pc_clear_jdata_cards


                         !!!!!!!!!!!!!!!!!!!!!!!


      function pc_num_gui_cards () result (ncards)
      integer                      :: ncards         ! result

      call pc_private_exists     ('pc_num_gui_cards')
!!!   call pc_private_assert     ('pc_num_gui_cards')
      ncards = cardset_num_cards (obj%gui)
      end function pc_num_gui_cards


      subroutine pc_alloc_gui_cards (pcards,ncards)
      character(len=*),pointer     :: pcards(:)      ! argument
      integer         ,intent(out) :: ncards         ! argument

      call pc_private_exists   ('pc_alloc_gui_cards')
!!!   call pc_private_assert   ('pc_alloc_gui_cards')
      call cardset_alloc_cards (obj%gui, pcards, ncards)
      end subroutine pc_alloc_gui_cards


      subroutine pc_get_gui_cards (cards,ncards,errmsg)
      character(len=*),intent(out) :: cards(:)       ! argument
      integer         ,intent(out) :: ncards         ! argument
      character(len=*),intent(out) :: errmsg         ! argument

      call pc_private_exists ('pc_get_gui_cards')
!!!   call pc_private_assert ('pc_get_gui_cards')
      call cardset_get_cards (obj%gui, cards, ncards, errmsg)
      end subroutine pc_get_gui_cards


      subroutine pc_get_gui_card (icard,card,errmsg)
      integer         ,intent(in)  :: icard          ! argument
      character(len=*),intent(out) :: card           ! argument
      character(len=*),intent(out) :: errmsg         ! argument

      call pc_private_exists ('pc_get_gui_card')
!!!   call pc_private_assert ('pc_get_gui_card')
      call cardset_get_card  (obj%gui, icard, card, errmsg)
      end subroutine pc_get_gui_card


      subroutine pc_put_gui_cards (cards,ncards)
      character(len=*),intent(in)  :: cards(:)       ! argument
      integer         ,intent(in)  :: ncards         ! argument

      call pc_private_assert ('pc_put_gui_cards')
      call cardset_put_cards (obj%gui, cards, ncards)
      end subroutine pc_put_gui_cards


      subroutine pc_put_gui_card (card)
      character(len=*),intent(in) :: card                ! argument

      call pc_private_assert ('pc_put_gui_card')
      call cardset_put_card  (obj%gui, card)
      end subroutine pc_put_gui_card


      subroutine pc_add_gui_card (card)
      character(len=*),intent(in) :: card                ! argument

      call pc_private_assert ('pc_add_gui_card')
      call cardset_add_card  (obj%gui, card)
      end subroutine pc_add_gui_card


      subroutine pc_clear_gui_cards

!!!   call pc_private_assert ('pc_clear_gui_cards')
      call cardset_clear     (obj%gui)
      end subroutine pc_clear_gui_cards


!!------------------------ get keyword information -----------------------!!
!!------------------------ get keyword information -----------------------!!
!!------------------------ get keyword information -----------------------!!


      function pc_process_keyword_present (keyword) result (present)
      character(len=*),intent(in) :: keyword       ! argument
      logical                     :: present       ! result

      call pc_private_assert            ('pc_process_keyword_present')
      present = cardset_keyword_present (obj%process, keyword)
      end function pc_process_keyword_present



      function pc_num_process_keywords () result (num)
      integer                     :: num       ! result

      call pc_private_assert     ('pc_num_process_keywords')
      num = cardset_num_keywords (obj%process)
      end function pc_num_process_keywords



      function pc_get_process_keyword (indx) result (keyword)
      integer,intent(in)            :: indx          ! argument
      character(len=PC_LENGTH)      :: keyword       ! result

      call pc_private_assert        ('pc_get_process_keyword')
      keyword = cardset_get_keyword (obj%process, indx)
      end function pc_get_process_keyword



      subroutine pc_remove_process_keyword (keyword)
      character(len=*),intent(in) :: keyword       ! argument

      call pc_private_assert      ('pc_remove_process_keyword')
      call cardset_remove_keyword (obj%process, keyword)
      end subroutine pc_remove_process_keyword


                         !!!!!!!!!!!!!!!!!!!!!!!!!


      function pc_pdata_keyword_present (keyword) result (present)
      character(len=*),intent(in) :: keyword       ! argument
      logical                     :: present       ! result

      call pc_private_exists            ('pc_pdata_keyword_present')
      present = cardset_keyword_present (obj%pdata, keyword)
      end function pc_pdata_keyword_present



      function pc_num_pdata_keywords () result (num)
      integer                     :: num       ! result

      call pc_private_exists     ('pc_num_pdata_keywords')
      num = cardset_num_keywords (obj%pdata)
      end function pc_num_pdata_keywords



      function pc_get_pdata_keyword (indx) result (keyword)
      integer,intent(in)            :: indx          ! argument
      character(len=PC_LENGTH)      :: keyword       ! result

      call pc_private_exists        ('pc_get_pdata_keyword')
      keyword = cardset_get_keyword (obj%pdata, indx)
      end function pc_get_pdata_keyword



      subroutine pc_remove_pdata_keyword (keyword)
      character(len=*),intent(in) :: keyword       ! argument

      call pc_private_assert      ('pc_remove_pdata_keyword',1)
      call cardset_remove_keyword (obj%pdata, keyword)
      end subroutine pc_remove_pdata_keyword


                         !!!!!!!!!!!!!!!!!!!!!!!!!


      function pc_jdata_keyword_present (keyword) result (present)
      character(len=*),intent(in) :: keyword       ! argument
      logical                     :: present       ! result

      call pc_private_exists            ('pc_jdata_keyword_present')
      present = cardset_keyword_present (obj%jdata, keyword)
      end function pc_jdata_keyword_present



      function pc_num_jdata_keywords () result (num)
      integer                     :: num       ! result

      call pc_private_exists     ('pc_num_jdata_keywords')
      num = cardset_num_keywords (obj%jdata)
      end function pc_num_jdata_keywords



      function pc_get_jdata_keyword (indx) result (keyword)
      integer,intent(in)            :: indx          ! argument
      character(len=PC_LENGTH)      :: keyword       ! result

      call pc_private_exists        ('pc_get_jdata_keyword')
      keyword = cardset_get_keyword (obj%jdata, indx)
      end function pc_get_jdata_keyword



      subroutine pc_remove_jdata_keyword (keyword)
      character(len=*),intent(in) :: keyword       ! argument

      call pc_private_assert      ('pc_remove_jdata_keyword',2)
      call cardset_remove_keyword (obj%jdata, keyword)
      end subroutine pc_remove_jdata_keyword


                         !!!!!!!!!!!!!!!!!!!!!!!!!


      function pc_global_keyword_present (keyword) result (present)
      character(len=*),intent(in) :: keyword       ! argument
      logical                     :: present       ! result

      call pc_private_assert            ('pc_global_keyword_present')
      present = cardset_keyword_present (obj%global, keyword)
      end function pc_global_keyword_present



      function pc_num_global_keywords () result (num)
      integer                     :: num       ! result

      call pc_private_assert     ('pc_num_global_keywords')
      num = cardset_num_keywords (obj%global)
      end function pc_num_global_keywords



      function pc_get_global_keyword (indx) result (keyword)
      integer,intent(in)            :: indx          ! argument
      character(len=PC_LENGTH)      :: keyword       ! result

      call pc_private_assert        ('pc_get_global_keyword')
      keyword = cardset_get_keyword (obj%global, indx)
      end function pc_get_global_keyword



      subroutine pc_remove_global_keyword (keyword)
      character(len=*),intent(in) :: keyword       ! argument

      call pc_private_assert      ('pc_remove_global_keyword')
      call cardset_remove_keyword (obj%global, keyword)
      end subroutine pc_remove_global_keyword


                         !!!!!!!!!!!!!!!!!!!!!!!!!


      function pc_gui_action_present (keyword,action) result (present)
      character(len=*),intent(in) :: keyword       ! argument
      character(len=*),intent(in) :: action        ! argument
      logical                     :: present       ! result
      character(len=PC_LENGTH)    :: bigword       ! local

      call pc_private_exists            ('pc_gui_action_present')
!!!   call pc_private_assert            ('pc_gui_action_present')
      call pc_private_combine           (keyword, action,   bigword)
      present = cardset_keyword_present (obj%gui, bigword)
      end function pc_gui_action_present



      function pc_num_gui_keywords () result (num)
      integer                     :: num       ! result

      call pc_private_exists     ('pc_num_gui_keywords')
!!!   call pc_private_assert     ('pc_num_gui_keywords')
      num = cardset_num_keywords (obj%gui)
      end function pc_num_gui_keywords



      function pc_get_gui_keyword (indx) result (keyword)
      integer,intent(in)            :: indx          ! argument
      character(len=PC_LENGTH)      :: keyword       ! result
      character(len=PC_LENGTH)      :: bigword       ! local
      character(len=PC_LENGTH)      :: action        ! local

      call pc_private_exists        ('pc_get_gui_keyword')
!!!   call pc_private_assert        ('pc_get_gui_keyword')
      bigword = cardset_get_keyword (obj%gui, indx)
      call pc_private_split         (bigword,   keyword, action)
      end function pc_get_gui_keyword



      function pc_get_gui_action (indx) result (action)
      integer,intent(in)            :: indx          ! argument
      character(len=PC_LENGTH)      :: action        ! result
      character(len=PC_LENGTH)      :: bigword       ! local
      character(len=PC_LENGTH)      :: keyword       ! local

      call pc_private_exists        ('pc_get_gui_action')
!!!   call pc_private_assert        ('pc_get_gui_action')
      bigword = cardset_get_keyword (obj%gui, indx)
      call pc_private_split         (bigword,   keyword, action)
      end function pc_get_gui_action



      subroutine pc_remove_gui_action (keyword,action)
      character(len=*),intent(in) :: keyword       ! argument
      character(len=*),intent(in) :: action        ! argument
      character(len=PC_LENGTH)    :: bigword       ! local

      call pc_private_exists      ('pc_remove_gui_action')
!!!   call pc_private_assert      ('pc_remove_gui_action')
      call pc_private_combine     (keyword, action,   bigword)
      call cardset_remove_keyword (obj%gui, bigword)
      end subroutine pc_remove_gui_action


                         !!!!!!!!!!!!!!!!!!!!!!!!!


      function pc_control_keyword_present (keyword) result (present)
      character(len=*),intent(in) :: keyword       ! argument
      logical                     :: present       ! result

      call pc_private_assert            ('pc_control_keyword_present')
      present = cardset_keyword_present (obj%control, keyword)
      end function pc_control_keyword_present



      function pc_num_control_keywords () result (num)
      integer                     :: num       ! result

      call pc_private_assert     ('pc_num_control_keywords')
      num = cardset_num_keywords (obj%control)
      end function pc_num_control_keywords



      function pc_get_control_keyword (indx) result (keyword)
      integer,intent(in)            :: indx          ! argument
      character(len=PC_LENGTH)      :: keyword       ! result

      call pc_private_assert        ('pc_get_control_keyword')
      keyword = cardset_get_keyword (obj%control, indx)
      end function pc_get_control_keyword



      subroutine pc_remove_control_keyword (keyword)
      character(len=*),intent(in) :: keyword       ! argument

      call pc_private_assert      ('pc_remove_control_keyword')
      call cardset_remove_keyword (obj%control, keyword)
      end subroutine pc_remove_control_keyword


!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!
!!----------------------- end of module ---------------------------------!!


      end module pc_module


!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!
!!----------------------------- end -------------------------------------!!

