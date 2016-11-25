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
! Name       : cfewb
! Category   : cfe
! Written    : 1999-08-03   by: Donna K. Vunderink
! Revised    : 2008-10-14   by: Bill Menger
! Maturity   : beta
! Purpose    : Allow the user to build and edit workfiles.
! Portability: No known limitations, but see note below.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! WORKFILE_BUILDER tabbed screen allows the user to build and edit workfiles,
! build jobfiles from workfiles and submit jobs to a batch queue.
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


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 62. 2008-10-14  Bill Menger  Changed documentation source to include suffix for files.
! 61. 2008-10-02  Bill Menger  Added MB3 option to view source code.
! 60. 2006-09-18  Goodger      Added printout of putsys_texec command when
!                              submitting a job.
!059. 2006-06-22  D. Glover    Added NULLIFY statements for Intel compiler.
! 58. 2006-01-10  B. Menger    Removed Unused Variables.
! 57. 2004-08-23  SMCook       Put trap code for old workfile name back in.
! 56. 2004-07-08  SMCook       Incorporated mfilebox (multi-file selection), and
!                               changed current workfile selection to be
!                               filterable.
! 55. 2004-05-26  SMCook       ICPS now uses '\xterm' to avoid confusion with
!                               an aliased version of 'xterm', and added code to
!                               use the proper ICPS command for the custom
!                               code case.
! 54. 2004-03-25  SMCook       ICPS now runs in background so GUI isn't locked.
!                              Commented fork code was removed.
! 53. 2004-03-17  SMCook       Now uses time stamp instead of pid to name icps
!                               log file -- interactive pid is invariant.
!                              Now supports both local and remote icps.
!                              Test code to support forked icps is present but
!                               commented -- multithreaded trials were unstable.
! 52. 2004-03-12  SMCook       Now creates/pipes to log file for icps.
! 51. 2004-03-11  SMCook       Added working directory to file specification
!                               for icps work file so file will be found if
!                               PWD changes as a result of the 'xon' step.
! 50. 2004-03-10  SMCook       Incorporated icps button.
! 49. 2004-01-29  SMCook       Reject filenames with interspersed periods.
! 48. 2003-11-18  Stoeckley    Provide workarounds for Portland Group compiler.
! 47. 2003-09-15  Stoeckley    Changed name from cfe_wb to cfewb;
!                               changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
! 46. 2002-09-23  Vunderink    Changed prints to writes for checkc.  Modifed
!                                SELECTCURRENT to display files only.
! 45. 2002-09-13  Vunderink    Added remove argument to cfeutil_read_file calls
! 44. 2002-09-11  Vunderink    Made changes for Select Current Workfile button.
! 43. 2002-09-10  Vunderink    Modified to use cfeprocess_delete_all_popups and
!                                added to cfewb_delete.
! 42. 2002-04-19  Vunderink    Moved checking for invalid process to fix insert
!                                blank.
! 41. 2002-04-18  Vunderink    Display error message if invalid process is
!                                selected for append, insert or replace.
! 40. 2002-03-07  Vunderink    Removed un-neccessary semi-colons.
! 39. 2001-02-26  Vunderink    Change current_trap to not use
!                                clear_current_workfile instead delete current
!                                workfile and any popups in current_trap.
! 38. 2001-02-26  Vunderink    Added clear_current_workfile to centralize
!                                clearing the current workfile and changed
!                                current_clear_trap and current_trap to use it.
!                                Also, set the FRONTEND_PATH before creating
!                                JOB_DATA processes.
! 37. 2001-01-16  Vunderink    Fixed insert problem when destination is above
!                                source
! 36. 2000-12-05  Vunderink    Removed error popup when the end of the old
!                                workfile buffer is reached.
! 35. 2000-11-09  Vunderink    Fixed replace problem associated with
!                                PROJECT_DATA and JOB_DATA.
! 34. 2000-10-16  Vunderink    Do finquire on current workfile to make sure it
!                                is writable.
! 33. 2000-10-11  Vunderink    Do finquire on old workfile.
! 32. 2000-10-08  Vunderink    Fixed globals when changing defaults  and
!                                removed HelpSection.
! 31. 2000-10-04  Vunderink    Fixed replace problem when source and
!                                destination are current workfile and
!                                destination is above source
! 30. 2000-09-18  Vunderink    Start running all traps at the process after the
!                                current process.
! 29. 2000-09-05  Vunderink    Check length of workfile name.
! 28. 2000-09-04  Vunderink    Release all memory on delete to aid in finding
!                                memory leaks.
! 27. 2000-08-21  Vunderink    On modify, check to see if process name has
!                                changed before deleting old process.
! 26. 2000-08-15  Vunderink    Removed use of cfe_constants, moved xxx_process
!                                routines into cfeprocess module, and changed
!                                character variables to use PC_LENGTH.
! 25. 2000-07-21  Vunderink    Fixed problem matching custom_xml with correct
!                                process, fixed ParmScreen, InsertBlank,
!                                InsertAfter, Replace, Append, and Delete to
!                                give error if extra empty row is selected,
!                                changed SubmitJob to build job if necessary,
!                                and fixed ParmScreen not to display any blank
!                                row.
! 24. 2000-06-13  Vunderink    Removed setting process maturity view
! 23. 2000-05-30  Vunderink    Change "SensitiveFiled" to "SensitiveButton" on
!                                bottom area buttons.
! 22. 2000-05-26  Vunderink    Changed fputline call
! 21. 2000-05-23  Vunderink    Added parameter cache calls to set minsize and
!                                maxsize of arrays.
! 20. 2000-05-11  Vunderink    Added ! to redirection of cfebld and cfesub
!                                for users with noclobber in their .cshrc
! 19. 2000-05-09  Vunderink    Make sure all pointer arrays are allocated,
!                                change project_defaults to reside in
!                                working_dir, add routine cfewb_subset_list to
!                                load subsetProcessList, fix bug in replacing
!                                JOB_DATA process (jobname was bad), made
!                                cfewb_current_trap check for workfile
!                                existance and read/load file if it already
!                                exist, and fixed bug so that error message will
!                                appear if left arrow button is pressed to go
!                                past the beginning of the workfile.
! 18. 2000-05-08  Vunderink    Made argument change in call to process_list
! 17. 2000-04-27  Vunderink    Fixed check on replacing processes
! 16. 2000-04-25  Vunderink    Clear color if user changes source process to
!                                different array, and fixed bug causing abort
!                                when replacing processes
! 15. 2000-04-24  Vunderink    Fixed bug causing abort when viewing multiple
!                                screen processes in the SubsetProcessList
! 14. 2000-04-24  Vunderink    Fixed ParmScreen button press to clear color from
!                                arrays.
! 13. 2000-04-24  Vunderink    Changed character variables to use named constant
!                                CFE_LENGTH, amde changes in cfewb_update to
!                                support new clicking design, fixed copyall_trap
!                                to report errors, made changes so that a
!                                current workfile is no longer required to look
!                                at processes in the old workfile or the all
!                                process list, keep track of last job built and
!                                last job submitted for edit/view menu options,
!                                fixed bug in current_list_trap, fixed
!                                create_process not to use removed modifyXXXX
!                                parameters, fixed insert_into_current_list to
!                                support inserting a blank row, and added
!                                routine check_from_mb3_popup as part of new
!                                clicking design.
! 12. 2000-03-17  Vunderink    Modified to use new process_module routines
!                                instead of super_module routines, and changed
!                                putsys_cmd call to putsys_texec call.
! 11. 2000-03-13  Vunderink    Added code to cfewb_copyall_trap to delete any
!                                popups associated with the current workfile.
! 10. 2000-03-08  Vunderink    Added routines cfewb_update_all_popups and
!                                cfewb_delete_popup, changed all code to use
!                                the new routines, change cfebld and cfesub
!                                commands to be built new variables prog_cfebld
!                                and prog_cfesub, changed use of CFE_JOBNAME to
!                                CFE_NAME, and delete any popups associated with
!                                a replace action.
!  9. 2000-03-02  Vunderink    Fixed problem in replacing project_data
!  8. 2000-02-29  Vunderink    Made changes to support updating all process
!                                popup windows when one is closed with OK or
!                                Apply.
!  7. 2000-02-27  Vunderink    Fixed refresh problem on selectold_trap by
!                                calling filebox_restore, fixed so that popup
!                                windows are closed when a processes is deleted,
!                                the old workfile buffer is cleared, and the
!                                current workfile is cleared.
!  6. 2000-02-24  Vunderink    Changed current_trap to use a cfeutil routine
!                                for validating the jobname
!  5. 2000-02-23  Vunderink    Added workfile builder help, changed delete to
!                                PasteElements action, and added call to
!                                super_set_view in subset_trap
!  4. 2000-02-16  Vunderink    Fixed buildjob_trap and submitjob_trap to issue
!                                error and return if there is no current
!                                workfile. Fixed current_trap to check for valid
!                                filename. Fixed buildjob_trap and
!                                submitjob_trap to display results of putsys_cmd
!                                in INFO dialog.  Fixed cfewb_create_process to
!                                pop up JOB_DATA and PROJECT_DATA from the
!                                SUBSETPROCESSLIST.
!  3. 2000-02-08  Vunderink    Fixed cfewb_update_process in global updating.
!  2. 2000-02-03  Vunderink    Added routine cfewb_delete_process, made copyall
!                                report errors, made undo report errors, removed
!                                hardcoded path to cfebld and cfesub executables
!                                and fixed bug in global updating.
!  1. 1999-08-03  Vunderink    Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! The Portland Group compiler incorrectly executes code such as the following:
!
!       subroutine glotch_get_next (aaa,next)
!       type(xxxx_struct),pointer :: aaa,next
!       next => aaa%next
!       end subroutine
!
!       call glotch_get_next (ccc,ccc)
!
! To work around the bug, calls such as the above (in this file and several
! others) have been changed to:
!
!       call glotch_get_next (ccc,next)
!       ccc => next
!
! Changing the subroutine like this does not fix the problem:
!
!       subroutine glotch_get_next (aaa,next)
!       type(xxxx_struct),pointer :: aaa,next
!       type(xxxx_struct),pointer :: temp
!       temp => aaa%next
!       next => temp
!       end subroutine
!
! The Portland Group compiler required modifications in the code as follows:
!
!       Replaced reverse INDEX intrinsic function calls with STRING_INDEX.
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


      module cfewb_module

      use cardset_module
      use cfejava_module
      use cfestruct_module
      use cfewindow_module
      use cfesl_module
      use cfegui_module
      use cfeutil_module
      use cfeprocess_module
      use cio_module
      use filebox_module
      use mfilebox_module
      use finquire_module
      use getsys_module
      use pc_module
      use process_module
      use putsys_module
      use string_module
      use unix_module
      use worklist_module
      use workfile_module
      use cnfg_module

      implicit none

      private
      public :: cfewb_create               ! uses the parameter cache.
      public :: cfewb_initialize
      public :: cfewb_update               ! uses the parameter cache.
      public :: cfewb_delete
      public :: cfewb_save_defaults
      public :: cfewb_get_defaults
      public :: cfewb_change_defaults
      public :: cfewb_subset_list
      public :: cfewb_clear_current_workfile
      public :: cfewb_append_to_worklist
      public :: cfewb_show_source

      character(len=100),public,save :: cfewb_ident = &
       '$Id: cfewb.f90,v 1.3 2008/10/02 19:59:57 mengewm Exp $'

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine cfewb_create (obj)
      implicit none
      type(cfestruct),pointer :: obj                   !argument


      nullify(obj%selectCurrentDialog)
      nullify(obj%mselectJobDialog)
      nullify(obj%oldProcessList)
      nullify(obj%currentProcessList)
      nullify(obj%subsetProcessList)
      nullify(obj%oldWorkfileList)
      nullify(obj%oldWorkfileDisplayed)
      nullify(obj%currentWorkfile)
      nullify(obj%undo_indexes)
      nullify(obj%sourceProcessId)
      nullify(obj%destinationProcessId)

      return
      end subroutine cfewb_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine cfewb_delete (obj)
      implicit none
      type(cfestruct),pointer        :: obj                           !argument



      call cfeprocess_delete_all_popups ()
      call worklist_delete               (obj%oldWorkfileList)
      call workfile_delete               (obj%currentWorkfile)
      call filebox_delete                (obj%selectCurrentDialog)
      call mfilebox_delete               (obj%mselectJobDialog)

      if (associated(obj%oldProcessList)    ) deallocate(obj%oldProcessList)
      if (associated(obj%currentProcessList)) deallocate(obj%currentProcessList)
      if (associated(obj%subsetProcessList) ) deallocate(obj%subsetProcessList)
      if (associated(obj%undo_indexes)      ) deallocate(obj%undo_indexes)
      if (associated(obj%sourceProcessId)   ) deallocate(obj%sourceProcessId)
      if (associated(obj%destinationProcessId))   &
                                            deallocate(obj%destinationProcessId)

      return
      end subroutine cfewb_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine cfewb_initialize (obj)
      implicit none
      type(cfestruct),pointer :: obj                                  !argument

      object => obj                                          ! needed for traps.

      allocate(obj%oldProcessList(1))
      allocate(obj%currentProcessList(1))
      allocate(obj%subsetProcessList(1))

      obj%processDefaults          = 'System Defaults'
      obj%insertButton             = .FALSE.
      obj%replaceButton            = .FALSE.
      obj%appendButton             = .TRUE.
      obj%deleteButton             = .FALSE.
      obj%oldJobName               = ' '
      obj%oldJobComment            = ' '
      obj%oldProcessList(1)        = ' '
      obj%NoldProcessList          = 0
      obj%currentJobName           = ' '
      obj%currentProcessList(1)    = ' '
      obj%NcurrentProcessList      = 0
      obj%processSubset            = 'All_Processes'
      obj%subsetProcessListControl = ' '
      obj%subsetProcessList(1)     = ' '
      obj%NsubsetProcessList       = 0

      obj%NprocessDefaultsMenu     = 3
      obj%processDefaultsMenu(1)   = 'System Defaults'
      obj%processDefaultsMenu(2)   = 'Project Defaults'
      obj%processDefaultsMenu(3)   = 'User Defaults'

      obj%NprocessSubsetMenu       = 17
      obj%processSubsetMenu(1)     = 'All_Processes'
      obj%processSubsetMenu(2)     = 'amplitude_mod'
      obj%processSubsetMenu(3)     = 'diagnostics'
      obj%processSubsetMenu(4)     = 'filters'
      obj%processSubsetMenu(5)     = 'headers'
      obj%processSubsetMenu(6)     = 'inversion'
      obj%processSubsetMenu(7)     = 'io'
      obj%processSubsetMenu(8)     = 'migrations'
      obj%processSubsetMenu(9)     = 'miscellaneous'
      obj%processSubsetMenu(10)    = 'multi_component'
      obj%processSubsetMenu(11)    = 'plot'
      obj%processSubsetMenu(12)    = 'sorts'
      obj%processSubsetMenu(13)    = 'stacks'
      obj%processSubsetMenu(14)    = 'statics'
      obj%processSubsetMenu(15)    = 'synthetics'
      obj%processSubsetMenu(16)    = 'transforms'
      obj%processSubsetMenu(17)    = 'velocity_Analysis'

      obj%undo_action              = ' '
      obj%undo_replace1            = 1
      obj%undo_replace2            = 2

      obj%NsourceProcessId         = 0
      obj%NdestinationProcessId    = 0

      call cfewb_subset_list (obj%lnklib,obj%processSubset,  &
                               obj%subsetProcessList,obj%NsubsetProcessList)

      return
      end subroutine cfewb_initialize


!!------------------------------ update ------------------------------------!!
!!------------------------------ update ------------------------------------!!
!!------------------------------ update ------------------------------------!!


      subroutine cfewb_update (obj)
      implicit none
      type(cfestruct),target  :: obj                                  !argument

      integer                   :: i                                   !local
      integer                   :: num_processes                       !local

      integer                   :: ncurrentJobName                     !local
      integer,allocatable       :: temp_array(:)                       !local
      integer                   :: isource_min                         !local
      integer                   :: isource_max                         !local
      integer                   :: idestination_min                    !local
      integer                   :: idestination_max                    !local

      character(len=PC_LENGTH)  :: ctemp                               !local
      character(len=20)         :: clear_keywords(3)                   !local
      integer                   :: nclear_keywords                     !local
      logical                   :: clear_arrays                        !local
      logical                   :: do_action                           !local

      object => obj                                          ! needed for traps.
      clear_arrays = .false.


!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!


      if (pc_gui_action_present('CFEApplication','EnterWindow')) then
        if (obj%last_wb_action_mb3) then
          call pc_put ('currentProcessList'   ,obj%currentProcessList ,  &
                                               obj%NcurrentProcessList)
          call pc_put ('processSubset'        ,obj%processSubset     )
          call pc_put ('subsetProcessList'    ,obj%subsetProcessList  ,  &
                                               obj%NsubsetProcessList)
          obj%last_wb_action_mb3 = .false.
        endif
        return
      endif
      obj%last_wb_action_mb3 = cfewb_check_from_mb3_popup()

      if (pc_gui_action_present('currentProcessList','ItemSelected')) then
        if (obj%NsourceProcessId .eq. 0) then
          call pc_alloc_gui ('currentProcessList','ItemSelected',  &
                              obj%sourceProcessId,obj%NsourceProcessId)
          if (obj%NsourceProcessId .eq. 0) return
          if (obj%sourceProcessId(1) .eq. INIL) then
            obj%NsourceProcessId = 0
            return
          endif
          obj%sourceKeyword = 'CURRENTPROCESSLIST'
          call pc_put_gui ('currentProcessList','SetArrayBackgroundColor',  &
                           'HLIGHT1')
        else
          call pc_alloc_gui ('currentProcessList','ItemSelected',  &
                              obj%destinationProcessId,            &
                              obj%NdestinationProcessId)
          if (obj%NdestinationProcessId .eq. 0) return
          if (obj%destinationProcessId(1) .eq. INIL) then
            obj%NdestinationProcessId = 0
            return
          endif
          call pc_put_gui ('currentProcessList','SetArrayBackgroundColor',  &
                           'HLIGHT2')
        endif
        return
      endif

      if (pc_gui_action_present('oldProcessList'    ,'ItemSelected')) then
        if (obj%NsourceProcessId .ne. 0) then
          if (trim(obj%sourceKeyword) .ne. 'OLDPROCESSLIST') then
            call pc_put ('currentProcessList',obj%currentProcessList ,  &
                                             obj%NcurrentProcessList)
            call pc_put ('subsetProcessList' ,obj%subsetProcessList  ,  &
                                             obj%NsubsetProcessList)
            call pc_put_gui (trim(obj%sourceKeyword),'ClearSelection','   ')
          endif
        endif
        call pc_alloc_gui ('oldProcessList','ItemSelected',  &
                            obj%sourceProcessId,obj%NsourceProcessId)
        if (obj%NsourceProcessId .eq. 0) return
        if (obj%sourceProcessId(1) .eq. INIL) then
          obj%NsourceProcessId = 0
          return
        endif
        obj%sourceKeyword = 'OLDPROCESSLIST'
        call pc_put_gui ('oldProcessList','SetArrayBackgroundColor',  &
                         'HLIGHT1')
        return
      endif

      if (pc_gui_action_present('subsetProcessList' ,'ItemSelected')) then
        if (obj%NsourceProcessId .ne. 0) then
          if (trim(obj%sourceKeyword) .ne. 'SUBSETPROCESSLIST') then
            call pc_put ('currentProcessList',obj%currentProcessList ,  &
                                              obj%NcurrentProcessList)
            call pc_put ('oldProcessList'    ,obj%oldProcessList  ,  &
                                              obj%NoldProcessList)
            call pc_put_gui (trim(obj%sourceKeyword),'ClearSelection','   ')
          endif
        endif
        call pc_alloc_gui ('subsetProcessList','ItemSelected',  &
                            obj%sourceProcessId,obj%NsourceProcessId)
        if (obj%NsourceProcessId .eq. 0) return
        if (obj%sourceProcessId(1) .eq. INIL) then
          obj%NsourceProcessId = 0
          return
        endif
        obj%sourceKeyword = 'SUBSETPROCESSLIST'
        call pc_put_gui ('subsetProcessList','SetArrayBackgroundColor',  &
                         'HLIGHT1')
        return
      endif

      if (pc_gui_action_present('SystemDefaultsMB3','ButtonPress')) then
        obj%processDefaults = 'System Defaults'
      else if (pc_gui_action_present('ProjectDefaultsMB3','ButtonPress')) then
        obj%processDefaults = 'Project Defaults'
      else if (pc_gui_action_present('UserDefaultsMB3','ButtonPress')) then
        obj%processDefaults = 'User Defaults'
      else
        call pc_get   ('processDefaults' ,obj%processDefaults)
      endif

      if (pc_gui_action_present('ProcessScreen'   ,'ButtonPress') .or.   &
          pc_gui_action_present('ProcessScreenMB3','ButtonPress')) then
        if (obj%NsourceProcessId .eq. 0) then
          call cfegui_beep()
          call pc_error ('No source selected')
          return
        endif
        call cfegui_jump_field ('ProcessScreen')
        do_action = .true.
        ctemp = obj%sourceKeyword
        call string_to_upper (ctemp)
        if (trim(ctemp) .eq. 'CURRENTPROCESSLIST') then
          call workfile_get_num_processes (obj%currentWorkfile,num_processes)
          if (obj%sourceProcessId(obj%NsourceProcessId) .gt. num_processes) then
            do_action = .false.
          endif
        endif
        if (do_action) then
          call cfeprocess_create (trim(obj%sourceKeyword),  &
                                  obj%sourceProcessId(obj%NsourceProcessId))
        else
          call cfegui_beep()
          call pc_error ('Extra empty row may not be selected')
        endif
        clear_arrays = .true.
        nclear_keywords = 1
        clear_keywords(1) = obj%sourceKeyword
        if (associated(obj%sourceProcessId)) deallocate(obj%sourceProcessId)
        if (associated(obj%destinationProcessId))   &
            deallocate(obj%destinationProcessId)
        obj%NsourceProcessId      = 0
        obj%NdestinationProcessId = 0
        call pc_put_gui ('currentProcessList','SetArrayBackgroundColor',  &
                         'HLIGHT3')
        if (clear_arrays) then
          do i=1,nclear_keywords
            call cfegui_clear_selection (trim(clear_keywords(i)))
          enddo
        endif
        return
      endif

      if (pc_gui_action_present('InsertBlank' ,'ButtonPress') .or.   &
          pc_gui_action_present('InsertBlankMB3','ButtonPress')) then
        if (obj%NsourceProcessId .eq. 0) then
          call cfegui_beep()
          call pc_error ('No source selected')
          return
        endif
        call pc_jump_field ('InsertBlank')
        call workfile_get_num_processes (obj%currentWorkfile,num_processes)
        if (obj%sourceProcessId(obj%NsourceProcessId) .gt. num_processes) then
          call cfegui_beep()
          call pc_error ('Extra empty row may not be selected')
        else
          allocate(temp_array(1))
          temp_array(1) = 0
          call cfewb_insert_into_current_list ('InsertBlank',        &
                                                temp_array,           &
                                                obj%sourceProcessId)
          deallocate(temp_array)
        endif
        clear_arrays = .true.
        nclear_keywords = 1
        clear_keywords(1) = 'currentProcessList'
        if (associated(obj%sourceProcessId)) deallocate(obj%sourceProcessId)
        if (associated(obj%destinationProcessId))   &
            deallocate(obj%destinationProcessId)
        obj%NsourceProcessId      = 0
        obj%NdestinationProcessId = 0
      endif

      if (pc_gui_action_present('CopyAll'   ,'ButtonPress') .or.   &
          pc_gui_action_present('CopyAllMB3','ButtonPress')) then
        call cfewb_copyall_trap ('CopyAll')
        call pc_jump_field ('CopyAll')
      endif

      if (pc_gui_action_present('InsertAfter'   ,'ButtonPress') .or.   &
          pc_gui_action_present('InsertAfterMB3','ButtonPress')) then
        if (obj%NsourceProcessId .eq. 0) then
          call cfegui_beep()
          call pc_error ('No source selected')
          return
        else if (obj%NdestinationProcessId .eq. 0) then
          call cfegui_beep()
          call pc_error ('No destination selected')
          return
        endif
        call pc_jump_field ('InsertAfter')
        do_action = .true.
        call workfile_get_num_processes (obj%currentWorkfile,num_processes)
        ctemp = obj%sourceKeyword
        call string_to_upper (ctemp)
        if (trim(ctemp) .eq. 'CURRENTPROCESSLIST') then
          do i = 1,obj%NsourceProcessId
            if (obj%sourceProcessId(i) .gt. num_processes) then
              do_action = .false.
              exit
            endif
          enddo
        endif
        do i = 1,obj%NdestinationProcessId
          if (obj%destinationProcessId(i) .gt. num_processes) then
            do_action = .false.
            exit
          endif
        enddo
        if (do_action) then
          call cfewb_insert_into_current_list (trim(obj%sourceKeyword),      &
                                                obj%sourceProcessId,          &
                                                obj%destinationProcessId)
        else
          call cfegui_beep()
          call pc_error ('Extra empty row may not be selected')
        endif
        if (trim(obj%sourceKeyword) .eq. 'CURRENTPROCESSLIST') then
          nclear_keywords = 1
          clear_keywords(1) = 'currentProcessList'
        else
          nclear_keywords = 2
          clear_keywords(1) = 'currentProcessList'
          clear_keywords(2) = obj%sourceKeyword
        endif
        clear_arrays = .true.
        if (associated(obj%sourceProcessId)) deallocate(obj%sourceProcessId)
        if (associated(obj%destinationProcessId))   &
            deallocate(obj%destinationProcessId)
        obj%NsourceProcessId      = 0
        obj%NdestinationProcessId = 0
      endif

      if (pc_gui_action_present('Replace'   ,'ButtonPress') .or.   &
          pc_gui_action_present('ReplaceMB3','ButtonPress')) then
        if (obj%NsourceProcessId .eq. 0) then
          call cfegui_beep()
          call pc_error ('No source selected')
          return
        else if (obj%NdestinationProcessId .eq. 0) then
          call cfegui_beep()
          call pc_error ('No destination selected')
          return
        endif
        call pc_jump_field ('Replace')
        ctemp = obj%sourceKeyword
        call string_to_upper (ctemp)
        if (trim(ctemp) .eq. 'CURRENTPROCESSLIST' .and.     &
            obj%sourceProcessId(obj%NsourceProcessId) .eq.  &
            obj%destinationProcessId(obj%NdestinationProcessId)) then
          call cfegui_beep()
          call pc_error ('Source and destination were the same')
        else
          do_action = .true.
          call workfile_get_num_processes (obj%currentWorkfile,num_processes)
          ctemp = obj%sourceKeyword
          call string_to_upper (ctemp)
          if (trim(ctemp) .eq. 'CURRENTPROCESSLIST') then
            do i = 1,obj%NsourceProcessId
              if (obj%sourceProcessId(i) .gt. num_processes) then
                do_action = .false.
                exit
              endif
            enddo
          endif
          do i = 1,obj%NdestinationProcessId
            if (obj%destinationProcessId(i) .gt. num_processes) then
              do_action = .false.
              exit
            endif
          enddo
          if (do_action) then
            if (trim(ctemp) .eq. 'CURRENTPROCESSLIST') then
              isource_min = minval(obj%sourceProcessId)
              isource_max = maxval(obj%sourceProcessId)
              idestination_min = minval(obj%destinationProcessId)
              idestination_max = maxval(obj%destinationProcessId)
              if ((idestination_min .le. isource_max .and.  &
                   idestination_min .ge. isource_min) .or.  &
                  (idestination_max .ge. isource_min .and.  &
                   idestination_max .le. isource_max)) then
                call cfegui_beep()
                call pc_error ('Source and Destination may not overlap')
                return
              endif
            endif
            call cfewb_replace_in_current_list (trim(obj%sourceKeyword),     &
                                                 obj%sourceProcessId,         &
                                                 obj%destinationProcessId)
          else
            call cfegui_beep()
            call pc_error ('Extra empty row may not be selected')
          endif
        endif
        if (trim(obj%sourceKeyword) .eq. 'CURRENTPROCESSLIST') then
          nclear_keywords = 1
          clear_keywords(1) = 'currentProcessList'
        else
          nclear_keywords = 2
          clear_keywords(1) = 'currentProcessList'
          clear_keywords(2) = obj%sourceKeyword
        endif
        clear_arrays = .true.
        if (associated(obj%sourceProcessId)) deallocate(obj%sourceProcessId)
        if (associated(obj%destinationProcessId))   &
            deallocate(obj%destinationProcessId)
        obj%NsourceProcessId      = 0
        obj%NdestinationProcessId = 0
      endif

      if (pc_gui_action_present('Append'   ,'ButtonPress') .or.   &
          pc_gui_action_present('AppendMB3','ButtonPress')) then
        if (obj%NsourceProcessId .eq. 0) then
          call cfegui_beep()
          call pc_error ('No source selected')
          return
        endif
        call pc_jump_field ('Append')
        do_action = .true.
        call workfile_get_num_processes (obj%currentWorkfile,num_processes)
        ctemp = obj%sourceKeyword
        call string_to_upper (ctemp)
        if (trim(ctemp) .eq. 'CURRENTPROCESSLIST') then
          do i = 1,obj%NsourceProcessId
            if (obj%sourceProcessId(i) .gt. num_processes) then
              do_action = .false.
              exit
            endif
          enddo
        endif
        if (do_action) then
          allocate(temp_array(1))
          temp_array(1) = obj%NcurrentProcessList
          call cfewb_insert_into_current_list (trim(obj%sourceKeyword),      &
                                                obj%sourceProcessId,          &
                                                temp_array)
          deallocate(temp_array)
        else
          call cfegui_beep()
          call pc_error ('Extra empty row may not be selected')
        endif
        nclear_keywords = 1
        clear_keywords(1) = obj%sourceKeyword
        clear_arrays = .true.
        if (associated(obj%sourceProcessId)) deallocate(obj%sourceProcessId)
        if (associated(obj%destinationProcessId))   &
            deallocate(obj%destinationProcessId)
        obj%NsourceProcessId      = 0
        obj%NdestinationProcessId = 0
      endif

      if (pc_gui_action_present('Delete'   ,'ButtonPress') .or.   &
          pc_gui_action_present('DeleteMB3','ButtonPress')) then
        if (obj%NsourceProcessId .eq. 0) then
          call cfegui_beep()
          call pc_error ('No source selected')
          return
        else if (trim(obj%sourceKeyword) .ne. 'CURRENTPROCESSLIST') then
          call cfegui_beep()
          call pc_error ('Can delete from current process list only')
          return
        endif
        call pc_jump_field ('Delete')
        do_action = .true.
        call workfile_get_num_processes (obj%currentWorkfile,num_processes)
        ctemp = obj%sourceKeyword
        call string_to_upper (ctemp)
        if (trim(ctemp) .eq. 'CURRENTPROCESSLIST') then
          do i = 1,obj%NsourceProcessId
            if (obj%sourceProcessId(i) .gt. num_processes) then
              do_action = .false.
              exit
            endif
          enddo
        endif
        if (do_action) then
          call cfewb_remove_from_current_list (obj%sourceProcessId)
        else
          call cfegui_beep()
          call pc_error ('Extra empty row may not be selected')
        endif
        nclear_keywords = 1
        clear_keywords(1) = obj%sourceKeyword
        clear_arrays = .true.
        if (associated(obj%sourceProcessId)) deallocate(obj%sourceProcessId)
        if (associated(obj%destinationProcessId))   &
            deallocate(obj%destinationProcessId)
        obj%NsourceProcessId      = 0
        obj%NdestinationProcessId = 0
      endif


      if (pc_gui_action_present('ClearSelection'   ,'ButtonPress') .or.  &
          pc_gui_action_present('ClearSelectionMB3','ButtonPress')) then
        if (obj%NsourceProcessId.gt.0 .and. obj%NdestinationProcessId.gt.0) then
          obj%NsourceProcessId      = 0
          obj%NdestinationProcessId = 0
          if (trim(obj%sourceKeyword) .eq. 'CURRENTPROCESSLIST') then
            nclear_keywords = 1
            clear_keywords(1) = 'currentProcessList'
          else
            nclear_keywords = 2
            clear_keywords(1) = 'currentProcessList'
            clear_keywords(2) = obj%sourceKeyword
          endif
          clear_arrays = .true.
        else if (obj%NsourceProcessId.gt.0) then
          obj%NsourceProcessId      = 0
          nclear_keywords = 1
          clear_keywords(1) = obj%sourceKeyword
          clear_arrays = .true.
        endif
        call pc_jump_field ('ClearSelection')
      endif

      if (pc_gui_action_present('Undo'   ,'ButtonPress') .or.  &
          pc_gui_action_present('UndoMB3','ButtonPress')) then
        call cfewb_undo_trap ('Undo')
      endif

      if (pc_gui_action_present('ClearCurrent'   ,'ButtonPress') .or.  &
          pc_gui_action_present('ClearCurrentMB3','ButtonPress')) then
        call cfewb_current_clear_trap ('ClearCurrent')
      endif

      if (pc_gui_action_present('ClearWorkfileBuffer'   ,'ButtonPress') .or.  &
          pc_gui_action_present('ClearWorkfileBufferMB3','ButtonPress')) then
        call cfewb_buffer_clear_trap ('ClearWorkfileBuffer')
      endif

      if (pc_gui_action_present('BuildJob'   ,'ButtonPress') .or.   &
          pc_gui_action_present('BuildJobMB3','ButtonPress')) then
        call cfewb_build_submit_job_trap ('BuildJob')
      endif

      if (pc_gui_action_present('ShowSourceMB3','ButtonPress')) then
        call cfewb_show_source(obj%sourceKeyword,obj%sourceProcessId,obj%NsourceProcessId)
      endif

      if (pc_gui_action_present('SubmitJob'   ,'ButtonPress') .or.   &
          pc_gui_action_present('SubmitJobMB3','ButtonPress')) then
        call cfewb_build_submit_job_trap ('SubmitJob')
      endif

      if (pc_gui_action_present('ICPSLocal' ,'ButtonPress') .or.   &
          pc_gui_action_present('ICPSLocalMB3','ButtonPress')) then
        call cfewb_icps_trap ('ICPS', .true.)
      endif

      if (pc_gui_action_present('ICPSRemote'   ,'ButtonPress') .or.   &
          pc_gui_action_present('ICPSRemoteMB3','ButtonPress')) then
        call cfewb_icps_trap ('ICPS', .false.)
      endif

      if (pc_gui_action_present('SelectJob','ButtonPress')) then
        call cfewb_selectold_trap ('SelectJob')
        return
      endif

      if (pc_gui_action_present('shiftOldLeft','ButtonPress')) then
        call cfewb_oldleft_trap ('shiftOldLeft')
      endif

      if (pc_gui_action_present('shiftOldRight','ButtonPress')) then
        call cfewb_oldright_trap ('shiftOldRight')
      endif

      if (pc_gui_action_present('SelectCurrent','ButtonPress')) then
        call cfewb_selectcurrent_trap ('SelectCurrent')
        return
      endif

      ctemp = obj%oldJobName
      call pc_get   ('oldJobName'             ,obj%oldJobName          ,  &
                                               cfewb_append_to_worklist2)
      if (len_trim(obj%oldJobName) .eq. 0) then
        if (len_trim(ctemp) .gt. 0) obj%oldJobName = ctemp
      endif

      call pc_get   ('oldJobComment'          ,obj%oldJobComment )

      call pc_alloc ('oldProcessList'         ,obj%oldProcessList      ,  &
                      obj%NoldProcessList)

      ctemp = obj%currentJobName
      call pc_get   ('currentJobName'         ,obj%currentJobName      ,  &
                                               cfewb_current_trap)
      if (len_trim(obj%currentJobName) .eq. 0) then
        if (len_trim(ctemp) .gt. 0) obj%currentJobName = ctemp
      endif

      call pc_alloc ('currentProcessList'     ,obj%currentProcessList  ,  &
                      obj%NcurrentProcessList ,cfewb_current_list_trap)

      call pc_get   ('processSubset'          ,obj%processSubset       ,  &
                                               cfewb_subset_trap)

      call pc_alloc ('subsetProcessList'      ,obj%subsetProcessList   ,  &
                      obj%NsubsetProcessList  ,cfewb_subset_list_trap)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!




!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!


      call pc_put_options_field ('processDefaults' ,obj%processDefaultsMenu , &
                                                    obj%NprocessDefaultsMenu)
      call pc_put_options_field ('processSubset'   ,obj%processSubsetMenu , &
                                                    obj%NprocessSubsetMenu)

      call pc_put_minsize_array ('oldProcessList'   ,obj%NoldProcessList   )
      call pc_put_maxsize_array ('oldProcessList'   ,obj%NoldProcessList   )
      call pc_put_minsize_array ('subsetProcessList',obj%NsubsetProcessList)
      call pc_put_maxsize_array ('subsetProcessList',obj%NsubsetProcessList)

      call pc_put ('processDefaults'      ,obj%processDefaults)
      call pc_put ('oldJobName'           ,obj%oldJobName         )
      call pc_put ('oldJobComment'        ,obj%oldJobComment      )
      call pc_put ('oldProcessList'       ,obj%oldProcessList     ,  &
                                           obj%NoldProcessList    )
      call pc_put ('currentJobName'       ,obj%currentJobName     )
      call pc_put ('currentProcessList'   ,obj%currentProcessList ,  &
                                           obj%NcurrentProcessList)
      call pc_put ('processSubset'        ,obj%processSubset     )
      call pc_put ('subsetProcessList'    ,obj%subsetProcessList  ,  &
                                           obj%NsubsetProcessList)

      call string_strip_blanks (obj%currentJobName,ncurrentJobName)
      if (obj%currentJobName(1:1) .eq. ' ') then
        call pc_put_sensitive_array_flag ('currentProcessList', .false.)
        call pc_put_minsize_array ('currentProcessList',obj%NcurrentProcessList)
        call pc_put_maxsize_array ('currentProcessList',obj%NcurrentProcessList)
      else
        call pc_put_sensitive_array_flag ('currentProcessList', .true.)
      endif

      if (clear_arrays) then
        do i=1,nclear_keywords
          call pc_put_gui (trim(clear_keywords(i)),'ClearSelection','   ')
        enddo
      endif

      return
      end subroutine cfewb_update


!!---------------------------- copyall_trap --------------------------------!!
!!---------------------------- copyall_trap --------------------------------!!
!!---------------------------- copyall_trap --------------------------------!!


      subroutine cfewb_copyall_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           :: i                           !local
      integer                           :: num_processes               !local





      if (.not. associated(object%currentWorkfile)) then
        call cfegui_beep()
        call pc_error ('Current Workfile name must be provided first')
        return
      endif

      call workfile_get_num_processes (object%currentWorkfile, num_processes)
      if (num_processes .gt. 2) then
        call cfegui_beep()
        call pc_error ('Current Workfile is not empty')
        return
      endif

      if (.not. associated(object%oldWorkfileDisplayed)) then
        call cfegui_beep()
        call pc_error ('There is no Old Workfile')
        return
      endif

      call cfeprocess_delete_all_popups()

      call workfile_copy (object%oldWorkfileDisplayed,object%currentWorkfile,  &
                          report_errors=.true.,report_warnings=.true.,         &
                          report_infos=.true.)
      call workfile_process_list (object%currentWorkfile,     &
                                  object%currentProcessList,  &
                                  object%NcurrentProcessList)
      call workfile_write (object%currentWorkfile,       &
                           trim(object%working_dir) //   &
                           trim(object%currentJobName))

      object%undo_action = 'REPLACE'
      if (associated(object%undo_indexes)) deallocate(object%undo_indexes)
      allocate(object%undo_indexes(object%NcurrentProcessList,2))
      object%undo_indexes = 0
      object%undo_indexes(1,object%undo_replace1) = 1
      object%undo_indexes(2,object%undo_replace1) = 2
      do i=1,object%NcurrentProcessList
        object%undo_indexes(i,object%undo_replace2) = i
      enddo

      return
      end subroutine cfewb_copyall_trap


!!------------------------------ undo_trap ---------------------------------!!
!!------------------------------ undo_trap ---------------------------------!!
!!------------------------------ undo_trap ---------------------------------!!


      subroutine cfewb_undo_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      type(process_struct),pointer      :: pcurrent                    !local
      type(process_struct),pointer      :: last_deleted                !local
      type(process_struct),pointer      :: previous                    !local
      type(process_struct),pointer      :: next                        !local
      character(len=PC_LENGTH)          :: process_name                !local
      character(len=PC_LENGTH)          :: job_name                    !local
      integer                           :: nundo_indexes               !local
      integer                           :: num_processes               !local
      integer                           :: i                           !local
      integer                           :: j                           !local

      nullify (last_deleted) ! jpa
      nullify (previous) ! jpa
      nullify (next) ! jpa
      nullify (pcurrent) ! jpa

      if (.not.associated(object%undo_indexes)) then
        call cfegui_beep()
        call pc_error ('Nothing to undo.')
        return
      endif

      select case (trim(object%undo_action))
        case ('INSERT')
          call workfile_clear_deleted  (object%currentWorkfile)
          nundo_indexes = size(object%undo_indexes(:,1))
          do i=1,nundo_indexes
            call workfile_get_first_process (object%currentWorkfile,pcurrent)
            do j = 2, object%undo_indexes(i,1)-i+1
              if (.not. associated(pcurrent)) exit
!!!           call process_get_next (pcurrent,pcurrent)
              call process_get_next (pcurrent,next)
              pcurrent => next
            enddo
            if (.not. associated(pcurrent)) then
              call cfegui_beep()
              call pc_error ('Error - Invalid process linked list - undo')
              return
            endif
            call workfile_delete_process (object%currentWorkfile,pcurrent)
          enddo
          object%undo_action = 'DELETE'

        case ('DELETE')
          call workfile_get_deleted   (object%currentWorkfile,last_deleted)
          if (.not.associated(last_deleted)) then
            call cfegui_beep()
            call pc_error ('Error - No previously deleted process found')
            object%undo_action = ' '
            return
          endif
          call workfile_reset_deleted (object%currentWorkfile)
          nundo_indexes = size(object%undo_indexes(:,1))
          do i=nundo_indexes,1,-1
            call workfile_get_first_process (object%currentWorkfile,pcurrent)
            do j=2,object%undo_indexes(i,1)-i
              if (.not. associated(pcurrent)) exit
!!!           call process_get_next (pcurrent,pcurrent)
              call process_get_next (pcurrent,next)
              pcurrent => next
            enddo
            if (.not. associated(pcurrent)) then
              call cfegui_beep()
              call pc_error ('Error - Invalid process linked list - undo')
              return
            endif
            call process_get_previous (last_deleted,previous)
            call workfile_insert_process (object%currentWorkfile,last_deleted, &
                                          pcurrent)
            if (associated(previous)) then
              last_deleted => previous
            else
              exit
            endif
          enddo
          object%undo_action = 'INSERT'

        case ('REPLACE')
          call workfile_get_deleted   (object%currentWorkfile,last_deleted)
          if (.not.associated(last_deleted)) then
            call cfegui_beep()
            call pc_error ('Error - No previously replaced process found')
            object%undo_action = ' '
            return
          endif
          call workfile_reset_deleted (object%currentWorkfile)
          nundo_indexes = size(object%undo_indexes(:,object%undo_replace2))
          do i=nundo_indexes,1,-1
            if (object%undo_indexes(i,object%undo_replace2) .eq. 0) cycle
            call workfile_get_first_process (object%currentWorkfile,pcurrent)
            do j = 2, object%undo_indexes(i,object%undo_replace2)
              if (.not. associated(pcurrent)) exit
!!!           call process_get_next (pcurrent,pcurrent)
              call process_get_next (pcurrent,next)
              pcurrent => next
            enddo
            if (.not. associated(pcurrent)) then
              call cfegui_beep()
              call pc_error ('Error - Invalid process linked list - undo')
              return
            endif
            call workfile_delete_process (object%currentWorkfile,pcurrent)
          enddo

          nundo_indexes = size(object%undo_indexes(:,object%undo_replace1))
          do i=1,nundo_indexes
            if (object%undo_indexes(i,object%undo_replace1) .eq. 0) cycle
            call process_get_previous (last_deleted,previous)
            call workfile_get_num_processes (object%currentWorkfile,  &
                                                                  num_processes)
            if (object%undo_indexes(i,object%undo_replace1) .gt. 1) then
              call workfile_get_first_process (object%currentWorkfile,pcurrent)
              do j=2,object%undo_indexes(i,object%undo_replace1)-1
                if (.not. associated(pcurrent)) exit
!!!             call process_get_next (pcurrent,pcurrent)
                call process_get_next (pcurrent,next)
                pcurrent => next
              enddo
              if (.not. associated(pcurrent)) then
                call cfegui_beep()
                call pc_error ('Error - Invalid process linked list - undo')
                return
              endif
              call workfile_insert_process (object%currentWorkfile,  &
                                            last_deleted,pcurrent)
            else
              call workfile_insert_first_process (object%currentWorkfile,  &
                                                  last_deleted)
            endif
            if (associated(previous)) then
              last_deleted => previous
            else
              exit
            endif
          enddo

          object%undo_action = 'REPLACE'
          i = object%undo_replace1
          object%undo_replace1 = object%undo_replace2
          object%undo_replace2 = i

        case default

      end select


      call workfile_get_num_processes (object%currentWorkfile,num_processes)
      object%NcurrentProcessList = size(object%currentProcessList)
      if (num_processes .ne. object%NcurrentProcessList) then
        if (associated(object%currentProcessList))  &
                                           deallocate(object%currentProcessList)
        object%NcurrentProcessList = num_processes
        allocate(object%currentProcessList(object%NcurrentProcessList))
      endif
      call workfile_get_first_process (object%currentWorkfile,pcurrent)
      i = 0
      do
        if (.not. associated(pcurrent)) exit
        call process_get_name(pcurrent,process_name)
        i = i + 1
        object%currentProcessList(i) = trim(process_name)
!!!     call process_get_next (pcurrent,pcurrent)
        call process_get_next (pcurrent,next)
        pcurrent => next
      enddo

      call workfile_get_first_process (object%currentWorkfile,pcurrent)
      i = 0
      do
        if (.not. associated(pcurrent)) exit
        i = i + 1
        call process_set_ipn (pcurrent,i)
        call process_get_name(pcurrent,process_name)
        if (trim(process_name) .eq. 'JOB_DATA') then
          call cfeutil_parse_for_jobname  (object%currentJobName,job_name)
          call process_put_process_cscalar (pcurrent,'JOBNAME',trim(job_name))
          call process_put_process_carray  (pcurrent,'PROCESS_LIST',      &
                                            object%currentProcessList,    &
                                            object%NcurrentProcessList)
          call process_update              (pcurrent,from_cards=.true.)
        endif
!!!     call process_get_next (pcurrent,pcurrent)
        call process_get_next (pcurrent,next)
        pcurrent => next
      enddo

      call workfile_run_all_traps(object%currentWorkfile,report_errors=.true.)
      call workfile_write (object%currentWorkfile,       &
                          trim(object%working_dir)//trim(object%currentJobName))
      call cfeprocess_update_all_popups()

      return
      end subroutine cfewb_undo_trap


!!-------------------------- current_clear_trap ----------------------------!!
!!-------------------------- current_clear_trap ----------------------------!!
!!-------------------------- current_clear_trap ----------------------------!!


      subroutine cfewb_current_clear_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      if (.not. associated(object%currentWorkfile)) then
        call cfegui_beep()
        call pc_error ('Current Workfile name must be provided first')
        return
      endif

      call cfewb_clear_current_workfile (object%currentJobName,      &
                                          object%currentProcessList,  &
                                          object%NcurrentProcessList, &
                                          object%currentWorkfile)

      return
      end subroutine cfewb_current_clear_trap


!!------------------------ clear_current_workfile --------------------------!!
!!------------------------ clear_current_workfile --------------------------!!
!!------------------------ clear_current_workfile --------------------------!!


      subroutine cfewb_clear_current_workfile (currentJobName,      &
                                                currentProcessList,  &
                                                NcurrentProcessList, &
                                                currentWorkfile)
      implicit none
      character(len=*),intent(inout)    ::  currentJobName             !argument
      character(len=*),pointer          ::  currentProcessList(:)      !argument
      integer         ,intent(inout)    ::  NcurrentProcessList        !argument
      type(workfile_struct),pointer     ::  currentWorkfile            !argument

      currentJobName      = ' '
      NcurrentProcessList = 0
      if (associated(currentProcessList)) then
        deallocate(currentProcessList)
        allocate(currentProcessList(1))
        currentProcessList(1) = ' '
      endif

      call workfile_delete (currentWorkfile)
      nullify(currentWorkfile)

      call cfeprocess_delete_all_popups()

      call pc_put_sensitive_array_flag ('currentProcessList', .false.)

      return
      end subroutine cfewb_clear_current_workfile


!!-------------------------- buffer_clear_trap -----------------------------!!
!!-------------------------- buffer_clear_trap -----------------------------!!
!!-------------------------- buffer_clear_trap -----------------------------!!


      subroutine cfewb_buffer_clear_trap (keyword)
      implicit none
      character(len=*),intent(in)       :: keyword                     !argument


      type(cfewindow_struct),pointer    :: window                      !local
      integer,pointer                   :: window_ids(:)               !local
      integer                           :: nwindow_ids                 !local
      integer                           :: i                           !local

      nullify (window) ! jpa

      object%oldJobName      = ' '
      object%oldJobComment   = ' '
      object%NoldProcessList = 0
      if (associated(object%oldProcessList)) then
        deallocate(object%oldProcessList)
        allocate(object%oldProcessList(1))
        object%oldProcessList(1) = ' '
      endif

      call worklist_delete (object%oldWorkfileList)
      nullify(object%oldWorkfileDisplayed)

      nullify(window_ids)
      call cfewindow_get_all_of_type ('OLDPROCESS',window_ids)
      if (associated(window_ids)) then
        nwindow_ids = size(window_ids)
        do i=1,nwindow_ids
          call cfegui_delete (window_ids(i),'Close')
          call cfewindow_get_pointer (window_ids(i),window)
          call cfewindow_delete (window)
        enddo
        deallocate(window_ids)
      endif

      return
      end subroutine cfewb_buffer_clear_trap


!!------------------------ build_submit_job_trap ---------------------------!!
!!------------------------ build_submit_job_trap ---------------------------!!
!!------------------------ build_submit_job_trap ---------------------------!!


      subroutine cfewb_build_submit_job_trap (keyword)
      implicit none
      character(len=*),intent(in)       :: keyword                     !argument

      integer                           :: istat                       !local
      integer                           :: i                           !local
      integer                           :: pid                         !local
      integer                           :: ncjn                        !local
      integer                           :: ncards                      !local
      character(len=10)                 :: cpid                        !local
      character(len=10)                 :: requestid                   !local
      character(len=1024)               :: cmd                         !local
      character(len=PC_LENGTH)          :: local_keyword               !local
      character(len=PC_LENGTH),pointer  :: cards(:)                    !local
      logical                           :: buildjob                    !local
      logical                           :: submitjob                   !local

      if (len_trim(object%currentJobName) .eq. 0) then
        call cfegui_beep()
        call pc_error ('There is no Current Workfile')
        return
      endif

      local_keyword = keyword
      call string_to_upper (local_keyword)

      pid = getsys_pid()
      call string_ii2cc (pid,cpid)

!!!   ncjn = index        (object%currentJobName,'.wrk',.TRUE.) - 1
      ncjn = string_index (object%currentJobName,'.wrk',.TRUE.) - 1

      select case (trim(local_keyword))
        case ('BUILDJOB')
          buildjob  = .true.
          submitjob = .false.
        case ('SUBMITJOB')
          buildjob  = .true.
          istat = finquire_input (trim(object%working_dir)//  &
                                  object%currentJobName(1:ncjn)//'.job')
          if (istat .eq. FINQUIRE_FOUND) then
            i = cfeutil_file_datecmp (trim(object%working_dir)//  &
                                      object%currentJobName(1:ncjn)//'.job', &
                                      trim(object%working_dir)//  &
                                      trim(object%currentJobName))
            if (i .gt. 0) buildjob  = .false.    !Jobfile is newer
          endif
          submitjob = .true.
      end select

      if (buildjob) then
        call workfile_run_all_traps(object%currentWorkfile,frontend=.true.,  &
                                    report_errors=.true.)

        cmd = trim(object%prog_cfebld)//' '//  &
              trim(object%working_dir)//object%currentJobName(1:ncjn)//' > .cfebld.'//trim(cpid)
        write(STDOUT,*) trim(cmd)
        call putsys_texec (trim(cmd))
        nullify(cards)
        call cfeutil_read_file ('.cfebld.'//trim(cpid),cards,ncards,  &
                                'Build Job Output',remove=.true.)
        if (ncards .gt. 0) then
          do i=1,ncards
            write(STDOUT,*) trim(cards(i))
            call pc_info (trim(cards(i)))
          enddo
          deallocate(cards)
        endif

        object%last_jobfile = trim(object%working_dir)//  &
                              object%currentJobName(1:ncjn)//'.job'

        call cfesl_add (object%currentJobName(1:ncjn),'BUILT')
      endif

      if (submitjob) then
        cmd = trim(object%prog_cfesub)//' '//  &
              trim(object%working_dir)//       &
              object%currentJobName(1:ncjn)//'.job'//  &
              ' > .cfesub.'//trim(cpid)

        write(STDOUT,*) trim(cmd)
        call putsys_texec (trim(cmd),istat)
        if (istat .ne. 0) then
          call cfegui_beep()
          call pc_error ('Error in putsys_texec with command ...')
          call pc_error (trim(cmd))

        endif

        nullify(cards)
        call cfeutil_read_file ('.cfesub.'//trim(cpid),cards,ncards,  &
                                'Submit Job Output',remove=.true.)
        if (ncards .gt. 0) then
          do i=1,ncards
            write(STDOUT,*)  trim(cards(i))
            call pc_info (trim(cards(i)))
          enddo
          call cfeutil_find_last_requestid (cards,ncards,requestid)
          deallocate(cards)
        endif

        if (len_trim(requestid) .ne. 0) then
          object%last_reportfile = trim(object%working_dir) //  &
                                   object%currentJobName(1:ncjn) //  &
                                   '.rpt.'//trim(requestid)
        endif

        call cfesl_add (object%currentJobName(1:ncjn),'SUBMITTED')
      endif

      return
      end subroutine cfewb_build_submit_job_trap


!!------------------------------- icps_trap --------------------------------!!
!!------------------------------- icps_trap --------------------------------!!
!!------------------------------- icps_trap --------------------------------!!


      subroutine cfewb_icps_trap (keyword, run_locally)
      implicit none
      character(len=*),intent(in)       :: keyword                    !argument
      logical                           :: run_locally                !argument

      character(len=512)                :: cmd                        !local
      character(len=256)                :: logfile                    !local
      character(len=80)                 :: stamp                      !local
      character(len=2)                  :: local_flag                 !local
      integer                           ::    ncjn  !local
      character(len=160)                :: xongp_bin

      call cnfg_get_value('bin_path_linux1',xongp_bin)
      xongp_bin = trim(xongp_bin)//'/xongp'
      if (object%exemode .ne. CFEJAVA_DIRECTAPP) then
        call pc_error( &
          'cfewb: The ICPS option is not available in client-server mode.')
        return
      end if

      if (len_trim(object%currentJobName) .eq. 0) then
        call cfegui_beep()
        call pc_error ('There is no Current Workfile')
        return
      endif

      ncjn = string_index (object%currentJobName,'.wrk',.TRUE.) - 1


      call string_time_date(stamp)
      call string_replace_character(stamp, ' ', '_')
      logfile = trim(object%working_dir) //            &
                      object%currentJobName(1:ncjn) // &
                      '.'//trim(stamp)//'.icps'

      if(run_locally) then
        local_flag = '-l'
      else
        local_flag = '-r'
      endif

      if(cfeutil_is_custom()) &
        object%prog_icps = trim(object%custom_path)//'/icps_script'

      cmd = '\xterm -sb -sl 5000 -g 100x50 -e '//trim(xongp_bin)//' '//local_flag//' "'// &
             trim(object%prog_icps)//' '                                 &
             //trim(object%working_dir)//trim(object%currentJobName)//   &
             ' | tee '//trim(logfile)//' "&'

      write(STDOUT,*) trim(cmd)
      !call putsys_texec ("pwd")
      call putsys_texec (trim(cmd))


      end subroutine cfewb_icps_trap


!!--------------------------- selectold_trap -------------------------------!!
!!--------------------------- selectold_trap -------------------------------!!
!!--------------------------- selectold_trap -------------------------------!!


      subroutine cfewb_selectold_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      character(len=PC_LENGTH)        :: local_key                     !local
      type(cfewindow_struct),pointer  :: window                        !local
      integer                         :: window_id                     !local

      nullify (window) ! jpa

      local_key = keyword
      call string_to_upper (local_key)

      call cfewindow_create           (window)
      call cfewindow_set_keyword      (window ,'OLDJOBNAME')
      call cfewindow_set_window_type  (window ,'MFILEBOX')
      call cfewindow_get_window_id    (window ,window_id)

      if (.not. associated(object%mselectJobDialog)) then
        call mfilebox_create (object%mselectJobDialog,  &
                             trim(object%working_dir) // '*.wrk')
      else
        call mfilebox_restore (object%mselectJobDialog)
      endif
      call cfegui_create (window_id,'FILE SELECTION','mfilebox.xml',.false.)
      call cfewindow_set_current (window)

      return
      end subroutine cfewb_selectold_trap


!!---------------------------- oldleft_trap --------------------------------!!
!!---------------------------- oldleft_trap --------------------------------!!
!!---------------------------- oldleft_trap --------------------------------!!


      subroutine cfewb_oldleft_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      type(workfile_struct),pointer   ::  previous                     !local
      type(cfewindow_struct),pointer  ::  window                       !local
      integer,pointer                 ::  window_ids(:)                !local
      integer                         ::  nwindow_ids                  !local
      integer                         ::  i                            !local

      nullify (previous) ! jpa
      nullify (window) ! jpa

      call workfile_get_previous (object%oldWorkfileDisplayed,previous)
      if (associated(previous)) then
        object%oldWorkfileDisplayed => previous
        call workfile_get_name (object%oldWorkfileDisplayed, object%oldJobName)
        call workfile_get_comment (object%oldWorkfileDisplayed,   &
                                   object%oldJobComment)
        call workfile_process_list (object%oldWorkfileDisplayed,  &
                                   object%oldProcessList,object%NoldProcessList)
        nullify(window_ids)
        call cfewindow_get_all_of_type ('OLDPROCESS',window_ids)
        if (associated(window_ids)) then
          nwindow_ids = size(window_ids)
          do i=1,nwindow_ids
            call cfegui_delete (window_ids(i),'Close')
            call cfewindow_get_pointer (window_ids(i),window)
            call cfewindow_delete (window)
          enddo
          deallocate(window_ids)
        endif
      else
        call cfegui_beep()
      endif

      return
      end subroutine cfewb_oldleft_trap


!!---------------------------- oldright_trap -------------------------------!!
!!---------------------------- oldright_trap -------------------------------!!
!!---------------------------- oldright_trap -------------------------------!!


      subroutine cfewb_oldright_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      type(workfile_struct),pointer   ::  next                         !local
      type(cfewindow_struct),pointer  ::  window                       !local
      integer,pointer                 ::  window_ids(:)                !local
      integer                         ::  nwindow_ids                  !local
      integer                         ::  i                            !local

      nullify (next) ! jpa
      nullify (window) ! jpa

      call workfile_get_next (object%oldWorkfileDisplayed,next)
      if (associated(next)) then
        object%oldWorkfileDisplayed => next
        call workfile_get_name (object%oldWorkfileDisplayed, object%oldJobName)
        call workfile_get_comment (object%oldWorkfileDisplayed,   &
                                   object%oldJobComment)
        call workfile_process_list (object%oldWorkfileDisplayed,  &
                                   object%oldProcessList,object%NoldProcessList)
        nullify(window_ids)
        call cfewindow_get_all_of_type ('OLDPROCESS',window_ids)
        if (associated(window_ids)) then
          nwindow_ids = size(window_ids)
          do i=1,nwindow_ids
            call cfegui_delete (window_ids(i),'Close')
            call cfewindow_get_pointer (window_ids(i),window)
            call cfewindow_delete (window)
          enddo
          deallocate(window_ids)
        endif
      else
        call cfegui_beep()
      endif

      return
      end subroutine cfewb_oldright_trap


!!------------------------------ old_trap (multi) --------------------------!!
!!------------------------------ old_trap (multi) --------------------------!!
!!------------------------------ old_trap (multi) --------------------------!!


      subroutine cfewb_append_to_worklist(fnames,n)
      implicit none
      character(len=*),pointer             :: fnames(:)              !argument
      integer,intent(in)                   :: n                      !argument
      integer                              :: i                      !local

      do i=1,n
        object%oldJobName = trim(fnames(i))
        call cfewb_append_to_worklist2(fnames(i))
      end do

      end subroutine cfewb_append_to_worklist


!!------------------------------ old_trap ----------------------------------!!
!!------------------------------ old_trap ----------------------------------!!
!!------------------------------ old_trap ----------------------------------!!


      subroutine cfewb_append_to_worklist2(fname)
      character(len=*),intent(in)     ::  fname                        !argument

      type(workfile_struct),pointer   ::  wcurrent,wnext               !local
      character(len=PC_LENGTH)        ::  wcurrent_name                !local

      integer                         ::  istat                        !local
      integer                         ::  i                            !local

      nullify (wcurrent) ! jpa
      nullify (wnext) ! jpa

      if (len_trim(object%oldJobName) .eq. 0) return

!!!   i = index        (trim(object%oldJobName),'/',.true.)
      i = string_index (trim(object%oldJobName),'/',.true.)
      if (i .eq. 0) then
        wcurrent_name = object%oldJobName
        object%oldJobName = trim(object%working_dir) // trim(wcurrent_name)
      endif

!!!   i = index        (object%oldJobName,'.wrk',.TRUE.)
      i = string_index (object%oldJobName,'.wrk',.TRUE.)
      if (i .eq. 0) then
        i = len_trim(object%oldJobName)
        object%oldJobName(i+1:) = '.wrk'
      endif

      istat = finquire_input(object%oldJobName)
      if (istat .eq. FINQUIRE_ERROR) then
        call cfegui_beep()
        call pc_error ('Could not open file '//trim(object%oldJobName))
        call cfegui_beep()
        object%oldJobName = ' '
        return
      endif

      if (.not. associated(object%oldWorkfileList)) then
        call worklist_create(object%oldWorkfileList)
        call workfile_create(object%oldWorkfileDisplayed)
        call worklist_set_first_workfile (object%oldWorkfileList,  &
                                          object%oldWorkfileDisplayed)
        call worklist_set_last_workfile  (object%oldWorkfileList,  &
                                          object%oldWorkfileDisplayed)
        call workfile_read (object%oldWorkfileDisplayed,  &
                            object%oldJobName)
      else
        nullify(object%oldWorkfileDisplayed)
        call worklist_get_first_workfile (object%oldWorkfileList,wcurrent)
        do
          if (.not. associated(wcurrent)) exit
          call workfile_get_name (wcurrent,wcurrent_name)
          if (trim(wcurrent_name) .eq. trim(object%oldJobName)) then
            object%oldWorkfileDisplayed => wcurrent
            exit
          endif
!!!       call workfile_get_next (wcurrent,wcurrent)
          call workfile_get_next (wcurrent,wnext)
          wcurrent => wnext
        enddo
        if (.not. associated(object%oldWorkfileDisplayed)) then
          call workfile_create(object%oldWorkfileDisplayed)
          call worklist_get_last_workfile (object%oldWorkfileList,wcurrent)
          call worklist_insert_workfile (object%oldWorkfileList,       &
                                         object%oldWorkfileDisplayed,  &
                                         wcurrent)
          call workfile_read (object%oldWorkfileDisplayed,  &
                              object%oldJobName)
        endif
      endif

      call workfile_get_name (object%oldWorkfileDisplayed, object%oldJobName)
      call workfile_get_comment (object%oldWorkfileDisplayed,  &
                                 object%oldJobComment)
      call workfile_process_list (object%oldWorkfileDisplayed,  &
                                  object%oldProcessList,object%NoldProcessList)

      return
      end subroutine cfewb_append_to_worklist2


!!------------------------- selectcurrent_trap -----------------------------!!
!!------------------------- selectcurrent_trap -----------------------------!!
!!------------------------- selectcurrent_trap -----------------------------!!


      subroutine cfewb_selectcurrent_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      character(len=PC_LENGTH)        :: local_key                     !local
      type(cfewindow_struct),pointer  :: window                        !local
      integer                         :: window_id                     !local

      nullify (window) ! jpa

      local_key = keyword
      call string_to_upper (local_key)

      call cfewindow_create           (window)
      call cfewindow_set_keyword      (window ,'CURRENTJOBNAME')
      call cfewindow_set_window_type  (window ,'FILEBOX')
      call cfewindow_get_window_id    (window ,window_id)

      if (.not. associated(object%selectCurrentDialog)) then
        call filebox_create (object%selectCurrentDialog,  &
                             trim(object%working_dir) // '*.wrk')
      else
        call filebox_restore (object%selectCurrentDialog)
      endif

      call cfegui_create (window_id,'FILE SELECTION','filebox.xml',.false.)
      call cfewindow_set_current (window)

      return
      end subroutine cfewb_selectcurrent_trap


!!---------------------------- current_trap --------------------------------!!
!!---------------------------- current_trap --------------------------------!!
!!---------------------------- current_trap --------------------------------!!


      subroutine cfewb_current_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      character(len=PC_LENGTH)          :: process_defaults            !local
      character(len=PC_LENGTH)          :: job_name                    !local
      character(len=PC_LENGTH),pointer  :: cards(:)                    !local
      character(len=PC_LENGTH)          :: msg                         !local
      integer                           :: ncards                      !local
      integer                           :: ncjn                        !local
      integer                           :: istat                       !local
      integer                           :: i                           !local
      type(process_struct),pointer      :: pfirst                      !local
      type(process_struct),pointer      :: plast                       !local

      nullify (pfirst) ! jpa
      nullify (plast) ! jpa

      call cfeutil_validate_jobname (object%currentJobName,istat,msg)
      if (istat .ne. CFE_NAME_VALID) then
        object%currentJobName = ' '
        call cfegui_beep()
        call pc_error (msg)
        return
      endif
      if (len_trim(object%currentJobName) .eq. 0) return

!!!   ncjn = index        (object%currentJobName,'.wrk',.TRUE.)
      ncjn = string_index (object%currentJobName,'.wrk',.TRUE.)
      if (ncjn .eq. 0) then
        ncjn = len_trim(object%currentJobName)
        object%currentJobName(ncjn+1:) = '.wrk'
      else
        ncjn = ncjn - 1
      endif

      if (len_trim(object%currentJobName) .gt. 19) then
        call cfegui_beep()
        call pc_error ('Workfile name too long. Maximum is 15 characters.')
        return
      endif

      i=string_index(object%currentJobName,'.');
      if (object%currentJobName(i+1:i+1) .ne. 'w') then
        call cfegui_beep()
        object%currentJobName = ''
        call pc_error ('Workfile name cannot contain interspersed periods.')
        return
      endif

      process_defaults = object%processDefaults
      call string_to_upper (process_defaults)

      istat = finquire_output (trim(object%working_dir) //  &
                               trim(object%currentJobName))
      if (istat .eq. FINQUIRE_ERROR) then
        call cfegui_beep()
        call pc_error ('Could not write file '//trim(object%currentJobName))
        call cfegui_beep()
        object%currentJobName = ' '
        return
      endif

      if (associated(object%currentWorkfile)) then
        call workfile_delete(object%currentWorkfile)
        call cfeprocess_delete_all_popups()
      endif

      call workfile_create(object%currentWorkfile)

      if (istat .eq. FINQUIRE_FOUND) then
        call workfile_read (object%currentWorkfile,  &
                            trim(object%working_dir)//object%currentJobName,  &
                            report_errors=.true.,report_warnings=.true.,  &
                            report_infos=.true.)
        call workfile_get_name (object%currentWorkfile,object%currentJobName)
        call workfile_process_list (object%currentWorkfile,      &
                                    object%currentProcessList,   &
                                    object%NcurrentProcessList)

      else
        call workfile_set_name (object%currentWorkfile,object%currentJobName)
        call process_create(pfirst,'PROJECT_DATA',1)
        call workfile_set_first_process (object%currentWorkfile,pfirst)
        call process_create(plast ,'JOB_DATA'    ,2)
        call process_put_process_cscalar(plast,'JOBNAME',  &
                                         object%currentJobName(1:ncjn))
        call workfile_set_last_process (object%currentWorkfile, plast)
        if (trim(process_defaults).eq.'PROJECT DEFAULTS' .or.  &
            trim(process_defaults).eq.'USER DEFAULTS') then
          nullify(cards)
          call cfewb_get_defaults (pfirst, process_defaults,   &
                                    object%working_dir, cards, ncards)
          if (ncards .gt. 0) then
            call process_put_gui_cards (pfirst, cards, ncards)
          endif
          call cfewb_get_defaults (plast, process_defaults,   &
                                    object%working_dir, cards, ncards)
          if (ncards .gt. 0) then
            call process_put_gui_cards  (plast, cards, ncards)
            call process_remove_keyword (plast,'GUI','JOBNAME#MODIFYFIELD')
          endif
          if (associated(cards)) deallocate(cards)
        endif
        call process_create_super        (pfirst)
        call process_copy_pdata_cards    (pfirst,plast)
        call process_copy_global_cards   (pfirst,plast)
        call process_put_process_cscalar (plast,'FRONTEND_PATH',        &
                                                       trim(object%working_dir))
        call process_create_super        (plast)
        call process_set_next            (pfirst, plast)
        call process_set_previous        (plast, pfirst)
        call workfile_set_num_processes  (object%currentWorkfile, 2)
        if (associated(object%currentProcessList))  &
                                           deallocate(object%currentProcessList)
        object%NcurrentProcessList = 2
        allocate(object%currentProcessList(object%NcurrentProcessList))
        object%currentProcessList(1) = 'PROJECT_DATA'
        object%currentProcessList(2) = 'JOB_DATA'
        call cfeutil_parse_for_jobname  (object%currentJobName,job_name)
        call process_put_process_cscalar (plast,'JOBNAME'     ,trim(job_name))
        call process_put_process_carray  (plast,'PROCESS_LIST',         &
                                          object%currentProcessList,    &
                                          object%NcurrentProcessList)
      endif

      call workfile_write (object%currentWorkfile,       &
                           trim(object%working_dir) //   &
                           trim(object%currentJobName))

      return
      end subroutine cfewb_current_trap


!!--------------------------- subset_trap ----------------------------------!!
!!--------------------------- subset_trap ----------------------------------!!
!!--------------------------- subset_trap ----------------------------------!!


      subroutine cfewb_subset_trap (keyword)
      implicit none
      character(len=*),intent(in)           :: keyword                ! argument

      call cfewb_subset_list (object%lnklib,object%processSubset,  &
                             object%subsetProcessList,object%NsubsetProcessList)


      return
      end subroutine cfewb_subset_trap


!!--------------------------- subset_trap ----------------------------------!!
!!--------------------------- subset_trap ----------------------------------!!
!!--------------------------- subset_trap ----------------------------------!!


      subroutine cfewb_subset_list (lnklib,category,list,nlist)
      implicit none
      integer         ,intent(in)           :: lnklib                 ! argument
      character(len=*),intent(in)           :: category               ! argument
      character(len=*),pointer              :: list(:)                ! argument
      integer         ,intent(out)          :: nlist                  ! argument


      call process_list (list,nlist,category)

      if (nlist .eq. 0) then
        call cfegui_beep()
        call pc_error ('No processes in category')
      endif

      return
      end subroutine cfewb_subset_list


!!------------------------ current_list_trap -------------------------------!!
!!------------------------ current_list_trap -------------------------------!!
!!------------------------ current_list_trap -------------------------------!!


      subroutine cfewb_current_list_trap (keyword,indx,action)
      implicit none
      character(len=*),intent(in)       :: keyword                     !argument
      integer         ,intent(in)       :: indx                        !argument
      integer         ,intent(in)       :: action                      !argument

      character(len=PC_LENGTH),pointer  :: cards(:)                    !local
      integer                           :: ncards                      !local
      character(len=PC_LENGTH)          :: process_name                !local
      character(len=PC_LENGTH)          :: job_name                    !local
      character(len=PC_LENGTH)          :: process_defaults            !local
      type(process_struct),pointer      :: pstart                      !local
      type(process_struct),pointer      :: pcurrent                    !local
      type(process_struct),pointer      :: pnew                        !local
      type(process_struct),pointer      :: previous,next               !local
      integer                           :: i                           !local
      integer                           :: window_id                   !local
      integer                           :: process_id                  !local

      nullify (cards) ! jpa
      nullify (previous) ! jpa
      nullify (next) ! jpa
      nullify (pcurrent) ! jpa
      nullify (pnew) ! jpa

      if (.not. associated(object%currentWorkfile)) then
        call cfegui_beep()
        call pc_error ('Current Workfile name must be provided first')
        return
      endif

      process_name = object%currentProcessList(indx)
      call string_to_upper (process_name)

      if (.not. cfewb_check_for_p_and_j_data(process_name,indx)) then
        call  workfile_process_list (object%currentWorkfile,     &
                                     object%currentProcessList,  &
                                     object%NcurrentProcessList)
        return
      endif

      if (trim(process_name) .eq. '' .and. action .eq. PC_MODIFY) then
        call cfegui_beep()
        call pc_error ('Blank is an invalid process name ')
        call  workfile_process_list (object%currentWorkfile,     &
                                     object%currentProcessList,  &
                                     object%NcurrentProcessList)
        return
      else if (trim(process_name) .ne. '') then
        if (.not. process_validate(process_name)) then
          call cfegui_beep()
          call pc_error ('Invalid process name '//trim(process_name))
          call  workfile_process_list (object%currentWorkfile,     &
                                       object%currentProcessList,  &
                                       object%NcurrentProcessList)
          return
        else
          call string_to_upper (object%currentProcessList(indx))
        endif
      endif

      nullify(pstart)

      select case (action)

        case (PC_INSERT)
          process_defaults = object%processDefaults
          call string_to_upper (process_defaults)

          call process_create(pnew,object%currentProcessList(indx),indx)
          call workfile_get_first_process (object%currentWorkfile, pcurrent)
          do i = 2, indx-1
            if (.not. associated(pcurrent)) exit
!!!         call process_get_next (pcurrent,pcurrent)
            call process_get_next (pcurrent,next)
            pcurrent => next
          enddo
          if (.not. associated(pcurrent)) then
            call cfegui_beep()
            call pc_error ('Error - Invalid process linked list')
            return
          endif
          call workfile_clear_deleted  (object%currentWorkfile)
          call workfile_insert_process (object%currentWorkfile,pnew,pcurrent)

          call process_copy_pdata_cards  (pcurrent, pnew)
          call process_copy_jdata_cards  (pcurrent, pnew)
          call process_copy_global_cards (pcurrent, pnew)
          if (trim(process_defaults).eq.'PROJECT DEFAULTS' .or.  &
              trim(process_defaults).eq.'USER DEFAULTS') then
            call cfewb_get_defaults (pnew, process_defaults,   &
                                      object%working_dir, cards, ncards)
            if (ncards .gt. 0) then
              call process_put_gui_cards (pnew, cards, ncards)
              if (trim(process_name) .eq. 'JOB_DATA') then
                call process_remove_keyword (pnew,'GUI','JOBNAME#MODIFYFIELD')
                call process_put_process_cscalar(pnew,'FRONTEND_PATH',     &
                                                       trim(object%working_dir))
              endif
            endif
          endif
          if (trim(object%currentProcessList(indx)) .ne. '') then
            call process_create_super (pnew)
          endif

          call process_get_next (pnew,pcurrent)
          i = indx
          do
            if (.not. associated(pcurrent)) exit
            i = i + 1
            call process_set_ipn (pcurrent,i)
!!!         call process_get_next (pcurrent,pcurrent)
            call process_get_next (pcurrent,next)
            pcurrent => next
          enddo

          pstart => pnew

          object%undo_action = 'INSERT'
          if (associated(object%undo_indexes)) deallocate(object%undo_indexes)
          allocate(object%undo_indexes(1,1))
          object%undo_indexes(1,1) = indx

        case (PC_REMOVE)
          call workfile_get_first_process (object%currentWorkfile, pcurrent)
          do i = 1, indx-1
            if (.not. associated(pcurrent)) exit
!!!         call process_get_next (pcurrent,pcurrent)
            call process_get_next (pcurrent,next)
            pcurrent => next
          enddo
          call workfile_clear_deleted  (object%currentWorkfile)
          call process_get_name (pcurrent,process_name)
          call process_get_ipn  (pcurrent,process_id)
          window_id = cfewindow_match ('PROCESS','CURRENTPROCESSLIST',  &
                                        process_name,process_id)
          if (window_id .ne. -1) call cfeprocess_delete_popup (window_id)
          call process_get_next(pcurrent,pstart)
          call workfile_delete_process (object%currentWorkfile,pcurrent)

          call workfile_get_first_process (object%currentWorkfile,pcurrent)
          i = 0
          do
            if (.not. associated(pcurrent)) exit
            i = i + 1
            call process_set_ipn (pcurrent,i)
!!!         call process_get_next (pcurrent,pcurrent)
            call process_get_next (pcurrent,next)
            pcurrent => next
          enddo

          object%undo_action = 'DELETE'
          if (associated(object%undo_indexes)) deallocate(object%undo_indexes)
          allocate(object%undo_indexes(1,1))
          object%undo_indexes(1,1) = indx

        case (PC_MODIFY)
          process_defaults = object%processDefaults
          call string_to_upper (process_defaults)

          call workfile_get_first_process (object%currentWorkfile,pcurrent)
          do i = 2, indx
            if (.not. associated(pcurrent)) exit
!!!         call process_get_next (pcurrent,pcurrent)
            call process_get_next (pcurrent,next)
            pcurrent => next
          enddo

          call process_get_name (pcurrent, process_name)
          if (trim(process_name) .eq. trim(object%currentProcessList(indx)))  &
            return
          call process_create(pnew,object%currentProcessList(indx),indx)
          call process_copy_pdata_cards  (pcurrent, pnew)
          call process_copy_jdata_cards  (pcurrent, pnew)
          call process_get_previous      (pcurrent, previous)
          call process_copy_global_cards (previous, pnew)
          if (trim(process_defaults).eq.'PROJECT DEFAULTS' .or.  &
              trim(process_defaults).eq.'USER DEFAULTS') then
            call cfewb_get_defaults (pnew, process_defaults,   &
                                      object%working_dir, cards, ncards)
            if (ncards .gt. 0) then
              call process_put_gui_cards (pnew, cards, ncards)
              if (trim(process_name) .eq. 'JOB_DATA') then
                call process_remove_keyword (pnew,'GUI','JOBNAME#MODIFYFIELD')
                call process_put_process_cscalar(pnew,'FRONTEND_PATH',     &
                                                       trim(object%working_dir))
              endif
            endif
          endif
          call process_create_super     (pnew)
          call workfile_clear_deleted   (object%currentWorkfile)
          call workfile_replace_process (object%currentWorkfile,pnew,pcurrent)
          call process_get_next         (pnew,pstart)

          object%undo_action = 'REPLACE'
          if (associated(object%undo_indexes)) deallocate(object%undo_indexes)
          allocate(object%undo_indexes(1,2))
          object%undo_indexes(1,1) = indx
          object%undo_indexes(1,2) = indx

      end select

      call workfile_get_first_process (object%currentWorkfile, pcurrent)
      do
        if (.not. associated(pcurrent)) exit
        call process_get_name (pcurrent,process_name)
        if (trim(process_name) .eq. 'JOB_DATA') then
          call cfeutil_parse_for_jobname  (object%currentJobName,job_name)
          call process_put_process_cscalar (pcurrent,'JOBNAME',trim(job_name))
          call process_put_process_carray  (pcurrent,'PROCESS_LIST',      &
                                            object%currentProcessList,    &
                                            object%NcurrentProcessList)
          call process_get_previous        (pcurrent,previous)
          call process_copy_global_cards   (previous,pcurrent)
          call process_update              (pcurrent,from_cards=.true.)
          exit
        endif
!!!     call process_get_next (pcurrent,pcurrent)
        call process_get_next (pcurrent,next)
        pcurrent => next
      enddo

      call workfile_run_all_traps(object%currentWorkfile,start=pstart)
      call workfile_write (object%currentWorkfile,       &
                           trim(object%working_dir) //   &
                           trim(object%currentJobName))
      call cfeprocess_update_all_popups()

      return
      end subroutine cfewb_current_list_trap


!!------------------------- subset_list_trap -------------------------------!!
!!------------------------- subset_list_trap -------------------------------!!
!!------------------------- subset_list_trap -------------------------------!!


      subroutine cfewb_subset_list_trap (keyword,indx,action)
      implicit none
      character(len=*),intent(in)       :: keyword                     !argument
      integer         ,intent(in)       :: indx                        !argument
      integer         ,intent(in)       :: action                      !argument


      return
      end subroutine cfewb_subset_list_trap


!!---------------------------- save_defaults -------------------------------!!
!!---------------------------- save_defaults -------------------------------!!
!!---------------------------- save_defaults -------------------------------!!


      subroutine cfewb_save_defaults (process, type, working_dir)
      implicit none
      type(process_struct),pointer      :: process                     !argument
      character(len=*),intent(in)       :: type                        !argument
      character(len=*),intent(in)       :: working_dir                 !argument

      character(len=PC_LENGTH)          :: local_type                  !local
      character(len=PC_LENGTH)          :: defaults_dir                !local
      character(len=PC_LENGTH)          :: process_name                !local
      character(len=PC_LENGTH)          :: home_dir                    !local
      character(len=PC_LENGTH)          :: filename                    !local
      character(len=PC_LENGTH),pointer  :: cards(:)                    !local
      integer                           :: ncards                      !local
      integer                           :: lun                         !local
      integer                           :: istat                       !local
      integer                           :: i                           !local
      logical                           :: exists                      !local
      character(len=2)                  :: mode                        !local
      character(len=133)                :: string                      !local
  !   integer                           :: nstring = 132               !local
      integer                           :: temp

      call getsys_env ('HOME',home_dir)
      local_type = type
      call string_to_upper (local_type)
      select case (trim(local_type))
        case ('USER DEFAULTS')
          defaults_dir = trim(home_dir)//'/.cfe_user_defaults'
        case ('PROJECT DEFAULTS')
          defaults_dir = trim(working_dir)//'/.cfe_project_defaults'
      end select

      call process_get_name (process,process_name)
      call string_to_lower (process_name)
      filename = trim(defaults_dir)//'/'//trim(process_name)//'.def'

      inquire (file=trim(defaults_dir),exist=exists)
      if (.not. exists) call putsys_texec ('mkdir -p '// trim(defaults_dir),istat)

      mode = "w"
      lun = cio_fopen(trim(filename),mode)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      string = '<PROCESS name="' // trim(process_name) // '">'
      temp = len_trim(string)
      istat = cio_fputline(string,temp,lun)
      nullify(cards)
      call process_alloc_gui_cards (process, cards, ncards)
      if (ncards .gt. 0) then
        string = ' <GUI>'
        temp = len_trim(string)
        istat = cio_fputline(string,temp,lun)
        do i=1,ncards
          string = '  ' // cards(i)
          temp = len_trim(string)
          istat = cio_fputline(string,temp,lun)
        enddo
        string = ' </GUI>'
        temp = len_trim(string)
        istat = cio_fputline(string,temp,lun)
      endif
      string = '</PROCESS>'
      temp = len_trim(string)
      istat = cio_fputline(string,temp,lun)
      istat = cio_fclose(lun)
      if (associated(cards)) deallocate(cards)

      return
      end subroutine cfewb_save_defaults


!!---------------------------- get_defaults --------------------------------!!
!!---------------------------- get_defaults --------------------------------!!
!!---------------------------- get_defaults --------------------------------!!


      subroutine cfewb_get_defaults (process, type, working_dir, cards, ncards)
      implicit none
      type(process_struct),pointer      :: process                     !argument
      character(len=*),intent(in)       :: type                        !argument
      character(len=*),intent(in)       :: working_dir                 !argument
      character(len=PC_LENGTH),pointer  :: cards(:)                    !argument
      integer                           :: ncards                      !argument

      character(len=PC_LENGTH)          :: local_type                  !local
      character(len=PC_LENGTH)          :: defaults_dir                !local
      character(len=PC_LENGTH)          :: process_name                !local
      character(len=PC_LENGTH)          :: home_dir                    !local
      character(len=PC_LENGTH)          :: filename                    !local
      character(len=PC_LENGTH)          :: tag_card_sav                !local
      character(len=PC_LENGTH),pointer  :: temp_cards(:)               !local
      integer                           :: lun                         !local
      integer                           :: istat                       !local
      character(len=2)                  :: mode                        !local
      character(len=133)                :: string                      !local
      integer,parameter                 :: nstring = 132               !local

      ncards = 0
      call getsys_env ('HOME',home_dir)
      local_type = type
      call string_to_upper (local_type)
      select case (trim(local_type))
        case ('USER DEFAULTS')
          defaults_dir = trim(home_dir)//'/.cfe_user_defaults'
        case ('PROJECT DEFAULTS')
          defaults_dir = trim(working_dir)//'/.cfe_project_defaults'
      end select

      call process_get_name (process,process_name)
      call string_to_lower (process_name)
      filename = trim(defaults_dir)//'/'//trim(process_name)//'.def'

      mode = "r"
      lun = cio_fopen(trim(filename),mode)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      if (associated(cards)) deallocate(cards)
      nullify(temp_cards)

      do
        istat = cio_fgetline(string,nstring,lun)
        if (istat .lt. 0) goto 100
        if (string(1:1) .eq. '<') then                       ! PROCESS card
          if (string(1:2) .eq. '</') exit                    ! end PROCESS card

        else if (string(1:2) .eq. ' <') then                 ! TAG card

          tag_card_sav = string

        else                                                 ! DATA card

          if (tag_card_sav(1:6) .eq. ' <GUI>') then
            if (ncards .gt. 0) then
              if (associated(temp_cards)) deallocate(temp_cards)
              allocate(temp_cards(ncards))
              temp_cards = cards
            endif
            if (associated(cards)) deallocate(cards)
            allocate(cards(ncards+1))
            if (ncards .gt. 0) cards(1:ncards) = temp_cards(1:ncards)
            ncards = ncards + 1
            cards(ncards) = string
          endif

        endif
      enddo
  100 continue

      if (associated(temp_cards)) deallocate(temp_cards)
      istat = cio_fclose(lun)

      return
      end subroutine cfewb_get_defaults


!!--------------------------- change_defaults ------------------------------!!
!!--------------------------- change_defaults ------------------------------!!
!!--------------------------- change_defaults ------------------------------!!


      subroutine cfewb_change_defaults (process)
      implicit none
      type(process_struct),pointer      :: process                     !argument

      type(process_struct),pointer      :: previous                    !local
      character(len=PC_LENGTH)          :: process_name                !local
      character(len=PC_LENGTH)          :: jobname                     !local
      character(len=PC_LENGTH)          :: process_defaults            !local
      character(len=PC_LENGTH),pointer  :: cards(:)                    !local
      integer                           :: ncards                      !local

      nullify (previous) ! jpa

      call process_get_name (process,process_name)

      call pc_get ('processDefaults' ,process_defaults)
      call string_to_upper (process_defaults)

      select case (trim(process_defaults))
        case ('SYSTEM DEFAULTS')
          if (trim(process_name) .eq. 'JOB_DATA') then
            call process_get_by_keyword (process,'PROCESS','JOBNAME',jobname)
            call pc_put_gui_only ('JOBNAME',jobname)
            call process_get_previous (process,previous)
            call process_copy_global_cards (previous,process)
            call process_initialize (process)
          else
            call process_get_previous (process,previous)
            call process_copy_global_cards (previous,process)
            call process_initialize (process)
          endif
        case ('PROJECT DEFAULTS','USER DEFAULTS')
          nullify(cards)
          call cfewb_get_defaults   (process, process_defaults,   &
                                      object%working_dir, cards, ncards)
          if (ncards .gt. 0) then
            call process_put_gui_cards (process, cards, ncards)
            if (trim(process_name) .eq. 'JOB_DATA') then
              call process_remove_keyword (process,'GUI','JOBNAME#MODIFYFIELD')
            endif
            call process_alloc_gui_cards (process,cards,ncards)
            call pc_put_gui_cards (cards,ncards)
            if (associated(cards)) deallocate(cards)
            nullify(previous)
            call process_get_previous (process,previous)
            call process_copy_global_cards (previous,process)
            call process_update (process)
          else
            call cfegui_beep()
            call pc_error('No '// trim(process_defaults) // ' found')
          endif
      end select

      return
      end subroutine cfewb_change_defaults


!!--------------------- insert_into_current_list ---------------------------!!
!!--------------------- insert_into_current_list ---------------------------!!
!!--------------------- insert_into_current_list ---------------------------!!


      subroutine cfewb_insert_into_current_list (keyword,src,des,replace)
      implicit none
      character(len=*),intent(in)       :: keyword                     !argument
      integer         ,intent(in)       :: src(:)                      !argument
      integer         ,intent(in)       :: des(:)                      !argument
      logical,optional                  :: replace                     !argument
      integer                           :: nsrc                        !local
      integer                           :: maxdes                      !local
      character(len=PC_LENGTH)          :: local_keyword               !local
      character(len=PC_LENGTH),pointer  :: cards(:)                    !local
      integer                           :: ncards                      !local
      character(len=PC_LENGTH),pointer  :: process_names(:)            !local
      character(len=PC_LENGTH)          :: process_name                !local
      character(len=PC_LENGTH)          :: process_defaults            !local
      character(len=PC_LENGTH)          :: job_name                    !local
      type(process_struct),pointer      :: psrc                        !local
      type(process_struct),pointer      :: pafter                      !local
      type(process_struct),pointer      :: pnew                        !local
      type(process_struct),pointer      :: pstart                      !local
      type(process_struct),pointer      :: next                        !local
      integer                           :: i                           !local
      integer                           :: j                           !local
      integer                           :: k                           !local

      nullify(cards)
      nullify(pstart)
      nullify (next) ! jpa
      nullify (pafter) ! jpa
      nullify (pnew) ! jpa
      nullify (psrc) ! jpa

      nsrc = size(src)
      if (.not. associated(object%currentWorkfile)) then
        call cfegui_beep()
        call pc_error ('Current Workfile name must be provided first')
        return
      endif

      maxdes = maxval(des)

      local_keyword = keyword
      call string_to_upper(local_keyword)

      allocate(process_names(nsrc))
      if (trim(local_keyword) .eq. 'CURRENTPROCESSLIST') then
        do i=1,nsrc
          process_names(i) = object%currentProcessList(src(i))
          if (.not. process_validate(process_names(i))) then
            call cfegui_beep()
            call pc_error('Invalid process '//process_names(i))
            if (associated(process_names)) deallocate(process_names)
            return
          endif
          write(STDOUT,*) 'insert process indx=',maxdes+i,  &
                          ' name=',process_names(i)
        enddo
      else if (trim(local_keyword) .eq. 'OLDPROCESSLIST') then
        do i=1,nsrc
          process_names(i) = object%oldProcessList(src(i))
          if (.not. process_validate(process_names(i))) then
            call cfegui_beep()
            call pc_error('Invalid process '//process_names(i))
            if (associated(process_names)) deallocate(process_names)
            return
          endif
          write(STDOUT,*) 'insert process indx=',maxdes+i,  &
                          ' name=',process_names(i)
        enddo
      else if (trim(local_keyword) .eq. 'SUBSETPROCESSLIST') then
        do i=1,nsrc
          process_names(i) = object%subsetProcessList(src(i))
          write(STDOUT,*) 'insert process indx=',maxdes+i,  &
                          ' name=',process_names(i)
        enddo
      else if (trim(local_keyword) .eq. 'INSERTBLANK') then
        do i=1,nsrc
          process_names(i) = ' '
          write(STDOUT,*) 'insert process indx=',maxdes+i,  &
                          ' name=',process_names(i)
        enddo
      endif

      do i=1,nsrc
        if (.not. cfewb_check_for_p_and_j_data(process_names(i),maxdes+i)) then
          if (associated(process_names)) deallocate(process_names)
          return
        endif
      enddo

      call cfewb_insert_array_elements (object%currentProcessList,  &
                                         object%NcurrentProcessList, &
                                         process_names,nsrc,maxdes+1)

      if (present(replace)) then
        if (.not.(replace)) then
          object%undo_action = 'INSERT'
          if (associated(object%undo_indexes)) deallocate(object%undo_indexes)
          allocate(object%undo_indexes(nsrc,1))
          do i=1,nsrc
            object%undo_indexes(i,1) = maxdes+i
          enddo
          call workfile_clear_deleted (object%currentWorkfile)
        else   ! undo_action = REPLACE
          do i=1,nsrc
            object%undo_indexes(i,object%undo_replace2) = maxdes+i
          enddo
        endif
      else
        object%undo_action = 'INSERT'
        if (associated(object%undo_indexes)) deallocate(object%undo_indexes)
        allocate(object%undo_indexes(nsrc,1))
        do i=1,nsrc
          object%undo_indexes(i,1) = maxdes+i
        enddo
        call workfile_clear_deleted (object%currentWorkfile)
      endif


      process_defaults = object%processDefaults
      call string_to_upper (process_defaults)

      select case (trim(local_keyword))

        case ('CURRENTPROCESSLIST')

          do i=1,nsrc
            call process_create(pnew,process_names(i),maxdes+i)

            call workfile_get_first_process (object%currentWorkfile,pafter)
            do j = 2, maxdes+i-1
              if (.not. associated(pafter)) exit
!!!           call process_get_next (pafter,pafter)
              call process_get_next (pafter,next)
              pafter => next
            enddo
            if (.not. associated(pafter)) then
              call cfegui_beep()
              call pc_error ('Error - Invalid process linked list')
              return
            endif

            call workfile_get_first_process (object%currentWorkfile,psrc)
            if (src(1).gt.des(1)) then      ! Inserting Above
              k = src(i)+i-1
            else
              k = src(i)
            endif
            do j = 2, k
              if (.not. associated(psrc)) exit
!!!           call process_get_next (psrc,psrc)
              call process_get_next (psrc,next)
              psrc => next
            enddo
            if (.not. associated(psrc)) then
              call cfegui_beep()
              call pc_error ('Error - Invalid process linked list')
              return
            endif

            call process_copy_pdata_cards   (pafter, pnew)
            call process_copy_jdata_cards   (pafter, pnew)
            call process_copy_global_cards  (pafter, pnew)
            call process_copy_process_cards (psrc  , pnew)
            call process_create_super (pnew)

            call workfile_insert_process (object%currentWorkfile,pnew,pafter)
            if (i .eq. 1) call process_get_next (pnew,pstart)
          enddo

        case ('OLDPROCESSLIST')

          do i=1,nsrc
            call process_create(pnew,process_names(i),maxdes+i)

            call workfile_get_first_process (object%oldWorkfileDisplayed,psrc)
            do j = 2, src(i)
              if (.not. associated(psrc)) exit
!!!           call process_get_next (psrc,psrc)
              call process_get_next (psrc,next)
              psrc => next
            enddo
            if (.not. associated(psrc)) then
              call cfegui_beep()
              call pc_error ('Error - Invalid process linked list')
              return
            endif

            if (maxdes+i .eq. 1) then           ! Inserting PROJECT_DATA

              call workfile_insert_first_process (object%currentWorkfile,pnew)
              call process_copy_process_cards (psrc, pnew)
              call process_create_super (pnew)
              if (i .eq. 1) call process_get_next (pnew,pstart)

            else

              call workfile_get_first_process (object%currentWorkfile,pafter)
              do j = 2, maxdes+i-1
                if (.not. associated(pafter)) exit
!!!             call process_get_next (pafter,pafter)
                call process_get_next (pafter,next)
                pafter => next
              enddo
              if (.not. associated(pafter)) then
                call cfegui_beep()
                call pc_error ('Error - Invalid process linked list')
                return
              endif
              call process_copy_pdata_cards   (pafter, pnew)
              call process_copy_jdata_cards   (pafter, pnew)
              call process_copy_global_cards  (pafter, pnew)
              call process_copy_process_cards (psrc  , pnew)
              if (trim(process_names(i)) .eq. 'JOB_DATA') then
                call process_put_process_cscalar(pnew,'FRONTEND_PATH',     &
                                                       trim(object%working_dir))
              endif
              call process_create_super (pnew)

              call workfile_insert_process (object%currentWorkfile,pnew,pafter)
              if (i .eq. 1) call process_get_next (pnew,pstart)
            endif
          enddo

        case ('SUBSETPROCESSLIST')

          do i=1,nsrc
            call process_create(pnew,process_names(i),maxdes+i)

            call workfile_get_first_process (object%currentWorkfile,pafter)
            do j = 2, maxdes+i-1
              if (.not. associated(pafter)) exit
!!!           call process_get_next (pafter,pafter)
              call process_get_next (pafter,next)
              pafter => next
            enddo
            if (.not. associated(pafter)) then
              call cfegui_beep()
              call pc_error ('Error - Invalid process linked list')
              return
            endif

            call process_copy_pdata_cards   (pafter, pnew)
            call process_copy_jdata_cards   (pafter, pnew)
            call process_copy_global_cards  (pafter, pnew)

            if (trim(process_defaults).eq.'PROJECT DEFAULTS' .or.  &
                trim(process_defaults).eq.'USER DEFAULTS') then
              call cfewb_get_defaults (pnew, process_defaults,   &
                                        object%working_dir, cards, ncards)
              if (ncards .gt. 0) then
                call process_put_gui_cards (pnew,cards,ncards)
              endif
            endif
            if (trim(process_names(i)) .eq. 'JOB_DATA') then
              call process_put_process_cscalar(pnew,'FRONTEND_PATH',     &
                                                       trim(object%working_dir))
            endif
            call process_create_super (pnew)
            if (associated(cards)) deallocate(cards)

            call workfile_insert_process (object%currentWorkfile,pnew,pafter)
            if (i .eq. 1) call process_get_next (pnew,pstart)
          enddo

        case ('INSERTBLANK')

          do i=1,nsrc
            call process_create(pnew,process_names(i),maxdes+i)

            call workfile_get_first_process (object%currentWorkfile,pafter)
            do j = 2, maxdes+i-1
              if (.not. associated(pafter)) exit
!!!           call process_get_next (pafter,pafter)
              call process_get_next (pafter,next)
              pafter => next
            enddo
            if (.not. associated(pafter)) then
              call cfegui_beep()
              call pc_error ('Error - Invalid process linked list')
              return
            endif

            call process_copy_pdata_cards   (pafter, pnew)
            call process_copy_jdata_cards   (pafter, pnew)
            call process_copy_global_cards  (pafter, pnew)

            call workfile_insert_process (object%currentWorkfile,pnew,pafter)
            if (i .eq. 1) call process_get_next (pnew,pstart)
          enddo

      end select
      if (associated(process_names)) deallocate(process_names)

      i = 0
      call workfile_get_first_process (object%currentWorkfile,pnew)
      do
        if (.not. associated(pnew)) exit
        i = i + 1
        call process_set_ipn (pnew,i)
        call process_get_name(pnew,process_name)
        if (trim(process_name) .eq. 'JOB_DATA') then
          call cfeutil_parse_for_jobname  (object%currentJobName,job_name)
          call process_put_process_cscalar (pnew,'JOBNAME'     ,trim(job_name))
          call process_put_process_carray  (pnew,'PROCESS_LIST',          &
                                            object%currentProcessList,    &
                                            object%NcurrentProcessList)
          call process_update              (pnew,from_cards=.true.)
        endif
!!!     call process_get_next (pnew,pnew)
        call process_get_next (pnew,next)
        pnew => next
      enddo

      call workfile_run_all_traps(object%currentWorkfile,start=pstart)
      call workfile_write (object%currentWorkfile,       &
                           trim(object%working_dir) //   &
                           trim(object%currentJobName))
      call cfeprocess_update_all_popups()

      return
      end subroutine cfewb_insert_into_current_list


!!--------------------- remove_from_current_list ---------------------------!!
!!--------------------- remove_from_current_list ---------------------------!!
!!--------------------- remove_from_current_list ---------------------------!!


      subroutine cfewb_remove_from_current_list (des)
      implicit none
      integer,pointer                   :: des(:)                      !argument

      type(process_struct),pointer      :: pstart                      !local
      type(process_struct),pointer      :: pcurrent                    !local
      type(process_struct),pointer      ::      next !local
      character(len=PC_LENGTH)          :: process_name                !local
      character(len=PC_LENGTH)          :: job_name                    !local
      integer                           :: ndes                        !local
      integer                           :: i                           !local
      integer                           :: j                           !local
      integer                           :: window_id                   !local
      integer                           :: process_id                  !local

      nullify (next) ! jpa
      nullify (pcurrent) ! jpa
      nullify (pstart) ! jpa

      if (.not. associated(object%currentWorkfile)) then
        call cfegui_beep()
        call pc_error ('Current Workfile name must be provided first')
        return
      endif

      if (.not. associated(des)) return

      ndes = size(des)
      do i=1,ndes
        if (des(i) .eq. 1) then
          call cfegui_beep()
          call pc_error ('PROJECT_DATA can not be removed')
          return
        else if (des(i) .eq. 2) then
          call cfegui_beep()
          call pc_error ('JOB_DATA can not be removed')
          return
        else
          call cfewb_remove_array_element (object%currentProcessList,  &
                                            object%NcurrentProcessList, &
                                            des(i)-i+1)
        endif
      enddo

      object%undo_action = 'DELETE'
      if (associated(object%undo_indexes)) deallocate(object%undo_indexes)
      allocate(object%undo_indexes(ndes,1))
      object%undo_indexes(:,1) = des
      call workfile_clear_deleted  (object%currentWorkfile)

      do i=1,ndes
        call workfile_get_first_process (object%currentWorkfile,pcurrent)
        do j = 2, des(i)-i+1
          if (.not. associated(pcurrent)) exit
!!!       call process_get_next (pcurrent,pcurrent)
          call process_get_next (pcurrent,next)
          pcurrent => next
        enddo
        call process_get_name (pcurrent,process_name)
        call process_get_ipn  (pcurrent,process_id)
        window_id = cfewindow_match ('PROCESS','CURRENTPROCESSLIST',  &
                                      process_name,process_id)
        if (window_id .ne. -1) call cfeprocess_delete_popup (window_id)
        call process_get_next (pcurrent,pstart)
        call workfile_delete_process (object%currentWorkfile,pcurrent)
      enddo

      call workfile_get_first_process (object%currentWorkfile,pcurrent)
      i = 0
      do
        if (.not. associated(pcurrent)) exit
        i = i + 1
        call process_set_ipn (pcurrent,i)
        call process_get_name(pcurrent,process_name)
        if (trim(process_name) .eq. 'JOB_DATA') then
          call cfeutil_parse_for_jobname  (object%currentJobName,job_name)
          call process_put_process_cscalar (pcurrent,'JOBNAME',trim(job_name))
          call process_put_process_carray  (pcurrent,'PROCESS_LIST',      &
                                            object%currentProcessList,    &
                                            object%NcurrentProcessList)
          call process_update              (pcurrent,from_cards=.true.)
        endif
!!!     call process_get_next (pcurrent,pcurrent)
        call process_get_next (pcurrent,next)
        pcurrent => next
      enddo

      call workfile_run_all_traps(object%currentWorkfile,start=pstart,  &
                                  report_errors=.true.)
      call workfile_write (object%currentWorkfile,       &
                          trim(object%working_dir)//trim(object%currentJobName))
      call cfeprocess_update_all_popups()

      return
      end subroutine cfewb_remove_from_current_list


!!---------------------- replace_in_current_list ---------------------------!!
!!---------------------- replace_in_current_list ---------------------------!!
!!---------------------- replace_in_current_list ---------------------------!!


      subroutine cfewb_replace_in_current_list (keyword,src,des)
      implicit none
      character(len=*),intent(in)       :: keyword                     !argument
      integer         ,pointer          :: src(:)                      !argument
      integer         ,pointer          :: des(:)                      !argument

      integer                           :: nsrc                        !local
      integer                           :: ndes                        !local
      integer                           :: i                           !local
      integer                           :: j                           !local
      character(len=PC_LENGTH)          :: process_name                !local
      logical                           :: found                       !local
      integer                           :: temp_array(1)               !local
      integer                           :: window_id                   !local
      integer                           :: process_id                  !local
      type(process_struct),pointer      :: pcurrent,next               !local

      nullify (next) ! jpa
      nullify (pcurrent) ! jpa

      if (.not. associated(object%currentWorkfile)) then
        call cfegui_beep()
        call pc_error ('Current Workfile name must be provided first')
        return
      endif

      if (.not. associated(src)) return
      if (.not. associated(des)) return

      nsrc = size(src)
      ndes = size(des)

      do i=1,ndes
        if (des(i) .eq. 1) then
          found = .false.
          do j=1,nsrc
            if (src(j) .eq. 1) then
              found = .true.
              exit
            endif
          enddo
          if (.not. found) then
            call cfegui_beep()
            call pc_error ('PROJECT_DATA not in both selections')
            return
          endif
        else if (des(i) .eq. 2) then
          found = .false.
          do j=1,nsrc
            if (src(j) .eq. 2) then
              found = .true.
              exit
            endif
          enddo
          if (.not. found) then
            call cfegui_beep()
            call pc_error ('JOB_DATA not in both selections')
            return
          endif
        endif
      enddo

      do i=1,nsrc
        if (src(i) .eq. 1) then
          found = .false.
          do j=1,ndes
            if (des(j) .eq. 1) then
              found = .true.
              exit
            endif
          enddo
          if (.not. found) then
            call cfegui_beep()
            call pc_error ('PROJECT_DATA not in both selections')
            return
          endif
        else if (src(i) .eq. 2) then
          found = .false.
          do j=1,ndes
            if (des(j) .eq. 2) then
              found = .true.
              exit
            endif
          enddo
          if (.not. found) then
            call cfegui_beep()
            call pc_error ('JOB_DATA not in both selections')
            return
          endif
        endif
      enddo

      do i=1,nsrc
        select case (trim(keyword))
          case ('CURRENTPROCESSLIST')
            if (.not. process_validate(object%currentProcessList(src(i)))) then
              call cfegui_beep()
              call pc_error  &
                         ('Invalid process '//object%currentProcessList(src(i)))
              return
            endif
          case ('OLDPROCESSLIST')
            if (.not. process_validate(object%oldProcessList(src(i)))) then
              call cfegui_beep()
              call pc_error ('Invalid process '//object%oldProcessList(src(i)))
              return
            endif
        end select
      enddo

      do i=ndes,1,-1
        call cfewb_remove_array_element (object%currentProcessList,  &
                                          object%NcurrentProcessList,des(i))
      enddo

      object%undo_action = 'REPLACE'
      if (associated(object%undo_indexes)) deallocate(object%undo_indexes)
      allocate(object%undo_indexes(max(nsrc,ndes),2))
      object%undo_indexes = 0
      object%undo_indexes(1:ndes,object%undo_replace1) = des
      call workfile_clear_deleted  (object%currentWorkfile)

      do i=ndes,1,-1
        call workfile_get_first_process (object%currentWorkfile,pcurrent)
        do j = 2,des(i)
          if (.not. associated(pcurrent)) exit
!!!       call process_get_next (pcurrent,pcurrent)
          call process_get_next (pcurrent,next)
          pcurrent => next
        enddo
        call process_get_name (pcurrent,process_name)
        call process_get_ipn  (pcurrent,process_id)
        window_id = cfewindow_match ('PROCESS','CURRENTPROCESSLIST',  &
                                      process_name,process_id)
        if (window_id .ne. -1) call cfeprocess_delete_popup (window_id)
        call workfile_delete_process (object%currentWorkfile,pcurrent)
      enddo

      call workfile_get_first_process (object%currentWorkfile,pcurrent)
      i = 0
      do
        if (.not. associated(pcurrent)) exit
        i = i + 1
        call process_set_ipn (pcurrent,i)
!!!     call process_get_next (pcurrent,pcurrent)
        call process_get_next (pcurrent,next)
        pcurrent => next
      enddo

!     Check if replacing above
      if (src(1).gt.des(1) .and. trim(keyword).eq.'CURRENTPROCESSLIST') then
        do i=1,nsrc
          src(i) = src(i) - ndes
        enddo
      endif

      temp_array(1) = maxval(des) - ndes
      call cfewb_insert_into_current_list (keyword,src,temp_array,  &
                                            replace=.true.)

      return
      end subroutine cfewb_replace_in_current_list


      subroutine cfewb_show_source(keyword,src,id)
        implicit none
        integer         ,pointer          :: src(:)
        character(len=*),intent(in)       :: keyword
        integer                           :: id
        character(len=64)                 :: source_file
        character(len=256)                :: source_base
        character(len=512)                :: cmd
        character(len=10)                 :: suffix

        if (id .eq. 0) then
          call cfegui_beep()
          call pc_error ('No process selected.  '//&
            'Try "Clear Selections" then click on a process in a list. '//&
            ' It will turn GREEN.  Now try again.')
          return
        endif
        call cnfg_get_value('cps_source_dir',source_base)
        call cnfg_get_value('cps_doc_suffix',suffix)
        select case(trim(keyword))
         case ('SUBSETPROCESSLIST')
           source_file=object%subsetProcessList(src(1))
         case ('CURRENTPROCESSLIST')
           source_file=object%currentProcessList(src(1))
         case ('OLDPROCESSLIST')
           source_file=object%oldProcessList(src(1))
         case default 
          call pc_error ('No process selected.  '//&
            'Try "Clear Selections" then click on a process in a list. '//&
            ' It will turn GREEN.  Now try again.')
        end select

        source_file = trim(source_file)//trim(suffix)
        call string_to_lower(source_file)
        source_base = trim(source_base)//'/'//trim(source_file)
        cmd = '\xterm -T ' &
              //trim(source_file)// &
              ' -geometry 88x48 -bg beige -fg black -e ' // &
              ' "vi +/descript_doc -m '  //trim(source_base)// &
              ' " &'
        !--WMM ADDED ABOVE TO REMOVE DEPENDENCY ON FIREFOX.
        !--    Of course, now you have a dependency on vi :^)

        !cmd = 'firefox file://'//trim(source_base)//' &'
        write(STDOUT,*) trim(cmd)
        call putsys_texec (trim(cmd))


      end subroutine cfewb_show_source

!!----------------------- insert_array_elements ----------------------------!!
!!----------------------- insert_array_elements ----------------------------!!
!!----------------------- insert_array_elements ----------------------------!!


      subroutine cfewb_insert_array_elements (array,narray,values,nvalues,indx)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer                                  :: narray               !argument
      character(len=*),intent(in)              :: values(:)            !argument
      integer                                  :: nvalues              !argument
      integer         ,intent(in)              :: indx                 !argument

      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (.not. associated(array)) then
        narray = indx + nvalues - 1
        allocate(array(narray))
        if (indx .gt. 1) array(1:indx-1) = ' '
        array(indx:indx+nvalues-1) = values
      else
        narray = size(array)
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray+nvalues))
        if (indx .gt. 1) then
          array(1:indx-1) = temp_array(1:indx-1)
        endif
        array(indx:indx+nvalues-1) = values(1:nvalues)
        array(indx+nvalues:narray+nvalues) = temp_array(indx:narray)
        narray = narray + nvalues
        deallocate(temp_array)
      endif

      return
      end subroutine cfewb_insert_array_elements


!!----------------------- remove_array_element -----------------------------!!
!!----------------------- remove_array_element -----------------------------!!
!!----------------------- remove_array_element -----------------------------!!


      subroutine cfewb_remove_array_element (array,narray,indx)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer                                  :: narray               !argument
      integer         ,intent(in)              :: indx                 !argument

      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (.not. associated(array)) return
      narray = size(array)
      if (indx .lt. 1 .or. indx .gt. narray) then
        call cfegui_beep()
        call pc_error ('Index out of range in remove_array_element')
        return
      endif
      if (narray .eq. 1) then
        nullify(array)
        narray = 0
      else
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray-1))
        if (indx .gt. 1) then
          array(1:indx-1) = temp_array(1:indx-1)
        endif
        array(indx:narray-1) = temp_array(indx+1:narray)
        narray = narray - 1
        deallocate(temp_array)
      endif

      return
      end subroutine cfewb_remove_array_element


!!---------------------- check_for_p_and_j_data ----------------------------!!
!!---------------------- check_for_p_and_j_data ----------------------------!!
!!---------------------- check_for_p_and_j_data ----------------------------!!


      function cfewb_check_for_p_and_j_data (process,indx) result (valid)
      implicit none
      character(len=*),intent(in)              :: process              !argument
      integer         ,intent(in)              :: indx                 !argument
      logical                                  :: valid                !result

      valid = .FALSE.
      if (trim(process) .eq. 'PROJECT_DATA' .and. indx .ne. 1) then
        call cfegui_beep()
        call pc_error ('PROJECT_DATA must be the first process')
      else if (trim(process) .eq. 'JOB_DATA' .and. indx .ne. 2) then
        call cfegui_beep()
        call pc_error ('JOB_DATA must be the second process')
      else if (indx .eq. 1 .and. trim(process) .ne. 'PROJECT_DATA') then
        call cfegui_beep()
        call pc_error ('First process must be PROJECT_DATA')
      else if (indx .eq. 2 .and. trim(process) .ne. 'JOB_DATA') then
        call cfegui_beep()
        call pc_error ('Second process must be JOB_DATA')
      else
        valid = .TRUE.
      endif

      return
      end function cfewb_check_for_p_and_j_data


!!------------------------ check_from_mb3_popup ----------------------------!!
!!------------------------ check_from_mb3_popup ----------------------------!!
!!------------------------ check_from_mb3_popup ----------------------------!!


      function cfewb_check_from_mb3_popup () result (found)
      implicit none
      logical                                  :: found                !result

      integer                                  :: i                    !local
      integer                                  :: j                    !local
      integer                                  :: num_keywords         !local
      character(len=PC_LENGTH)                 :: keyword              !local
      character(len=PC_LENGTH)                 :: action               !local

      found = .FALSE.
      num_keywords = pc_num_gui_keywords()
      do i=1,num_keywords
        keyword = pc_get_gui_keyword(i)
        action = pc_get_gui_action(i)
        call string_to_upper (action)
        if (trim(action) .eq. 'BUTTONPRESS') then
          j = index(keyword,'MB3')
          if (j .gt. 0) then
            found = .TRUE.
            exit
          endif
        endif
      enddo

      return
      end function cfewb_check_from_mb3_popup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module cfewb_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

