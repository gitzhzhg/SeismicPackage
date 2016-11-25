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
! Name       : cfemenu
! Category   : cfe
! Written    : 1999-08-25   by: Donna K. Vunderink
! Revised    : 2007-10-16   by: Karen Goodger
! Maturity   : production
! Purpose    : CFE Pull-Down Menu Module.
! Portability: No known limitations, but see notes below.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! This primitive is used by cfe.
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
! 38. 2007-10-16  Goodger      Change command for running cpsdoc.  It is
!                              now on sipdocs.  Also, use firefox rather than
!                              netscape.
! 37. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
! 36. 2006-01-10  B. Menger    Removed Unused Variables.
! 35. 2004-08-23  SMCook       Incorporated mfilebox (multi-file selection).
! 34. 2003-11-18  Stoeckley    Replace reverse INDEX intrinsic functions with
!                               STRING_INDEX to make the Portland Group
!                               compiler happy.
! 33. 2003-09-15  Stoeckley    Changed name from cfe_menu to cfemenu;
!                               changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
! 32. 2002-10-17  Vunderink    Added access to ciu program.
! 31. 2002-09-23  Vunderink    Changed prints to writes for checkc and removed
!                                Ponca node dependencies.
! 30. 2002-09-11  Vunderink    Enabled JOBMON startup.
! 29. 2002-07-25  Vunderink    Modified to use cfejava_module
! 28. 2002-01-04  Vunderink    Added buildlist tool.
! 27. 2001-07-02  Vunderink    Added CPS header display.
! 26. 2001-02-26  Vunderink    Clear current workfile when changing working
!                                directory
! 25. 2001-02-16  Vunderink    Changed cpsDOC to no longer use pospx3
! 24. 2001-02-01  Vunderink    Added exec_node to user perferences and added
!                                support for starting Xtpioadmin, cbyt, va, csv
!                                and cfg
! 23. 2001-01-30  Vunderink    Update all filebox filters when working_dir
!                                changes
! 22. 2001-01-23  Vunderink    Added access to xpbs program
! 21. 2000-10-18  Vunderink    Moved userid into cfestruct
! 20. 2000-10-16  Vunderink    See if the user can write to the directory before
!                                changing the working directory.
! 19. 2000-09-14  Vunderink    Make sure htemp array is large enough for NULL
!                                terminator.
! 18. 2000-09-06  Vunderink    Put netscape in background and changed
!                                "Working Directory" to "Working Environment".
! 17. 2000-09-05  Vunderink    Added nedit to editor choices and added support
!                                for displaying DOC.
! 16. 2000-09-04  Vunderink    Eliminate un-necessary calls to string_to_upper
!                                and release all memory on delete to aid in
!                                finding memory leaks.
! 15. 2000-08-15  Vunderink    Removed use of cfe_constants, and changed
!                                character variable to PC_LENGTH
! 14. 2000-06-13  Vunderink    Moved initializing process_maturity to cfe
! 13. 2000-05-26  Vunderink    Check execution mode before forking an X 
!                                application and save preferences in session
!                                file.
! 12. 2000-05-23  Vunderink    Added parameter cache calls to set minsize and
!                                maxsize of arrays.
! 11. 2000-05-09  Vunderink    Make sure pointer arrays that could be sent to
!                                pc_put are allocated.  Use new cfewb routine
!                                to update the subsetProcessList.
! 10. 2000-04-24  Vunderink    Changed character variables to use named constant
!                                CFE_LENGTH, and added support for EDIT and VIEW
!                                menu options
!  9. 2000-03-20  Vunderink    Set processes to view to RAW for custom CFE
!  8. 2000-03-17  Vunderink    Modified to use new process_module routines
!                                instead of super_module routines
!  7. 2000-03-09  Vunderink    Add filebox_sort to cfemenu_load_cwd_dirs
!  6. 2000-03-08  Vunderink    Nullify cwd_dirs pointer, add error argument to
!                                cfemenu_execute, and return an error if the
!                                desired working directory does not exist.
!  5. 2000-02-29  Vunderink    Made ApplicationVersion call 
!                                cfegui_clientversion.
!  4. 2000-02-23  Vunderink    Modified Change Directory to use ItemSelected
!  3. 2000-02-16  Vunderink    Fixed file extention for filter parameters.
!                                Added updating working directory after exiting
!                                the Change Working Directory dialog.
!  2. 2000-02-08  Vunderink    Fixed bug in cfemenu_load_cwd_dirs when 
!                                directory is invalid.
!  1. 1999-08-25  Vunderink    Initial version.
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


      module cfemenu_module

      use buildlist_module
      use cardset_module
      use cfegui_module
      use cfemwb_module
      use cfestruct_module
      use cfeutil_module
      use cfewb_module
      use cfewindow_module
      use cfejava_module
      use cnfg_module
      use filebox_module
      use mfilebox_module
      use finquire_module
      use getsys_module
      use pc_module
      use process_module
      use putsys_module
      use string_module

      implicit none

      private
      public :: cfemenu_create
      public :: cfemenu_initialize
      public :: cfemenu_delete
      public :: cfemenu_option
      public :: cfemenu_update
      public :: cfemenu_execute

      character(len=100),public,save :: cfemenu_ident = &
       '$Id: cfemenu.f90,v 1.38 2007/10/16 18:01:30 Goodger prod sps $'


      contains


!!-------------------------------- create ----------------------------------!!
!!-------------------------------- create ----------------------------------!!
!!-------------------------------- create ----------------------------------!!


      subroutine cfemenu_create (obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      nullify (obj%buildjob_dialog)
      nullify (obj%editwork_dialog)
      nullify (obj%editjob_dialog)
      nullify (obj%editfile_dialog)
      nullify (obj%viewwork_dialog)
      nullify (obj%viewjob_dialog)
      nullify (obj%viewreport_dialog)
      nullify (obj%viewfile_dialog)
      nullify (obj%cwd_dirs)
      nullify (obj%custom_xml_temp)
      nullify (obj%custom_xml)
      nullify (obj%selectJobDialog) ! jpa
      nullify (obj%selectCurrentDialog) ! jpa
      nullify (obj%currentProcessList) ! jpa
      nullify (obj%subsetProcessList) ! jpa
      nullify (obj%currentWorkfile) ! jpa
      nullify (obj%SJjobs_dialog) ! jpa
      nullify (obj%templateJobDialog) ! jpa
      nullify (obj%masterBuffer) ! jpa

      return
      end subroutine cfemenu_create


!!------------------------------ initialize --------------------------------!!
!!------------------------------ initialize --------------------------------!!
!!------------------------------ initialize --------------------------------!!


      subroutine cfemenu_initialize (obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      integer                             :: i1                       !local
      integer                             :: i2                       !local
      character(len=PC_LENGTH)            :: fname                    !local

!!!   i1 = index        (obj%currentjobname,'/',.true.)
      i1 = string_index (obj%currentjobname,'/',.true.)
      if (i1 .eq. 0) i1 = 1
!!!   i2 = index        (obj%currentjobname,'.wrk',.true.)
      i2 = string_index (obj%currentjobname,'.wrk',.true.)
      if (i2 .eq. 0) i2 = len_trim(obj%currentjobname)
      fname = trim(obj%working_dir) // '/' // obj%currentjobname(i1:i2)

      allocate(obj%custom_xml_temp(1))
      allocate(obj%custom_xml(1))

      obj%buildjobfile          = trim(fname) // '.wrk'
      obj%editworkfile          = trim(fname) // '.wrk'
      obj%editjobfile           = trim(fname) // '.job'
      obj%editfile              = ' '
      obj%viewworkfile          = trim(fname) // '.wrk'
      obj%viewjobfile           = trim(fname) // '.job'
      obj%viewreportfile        = trim(fname) // '.rpt'
      obj%viewfile              = ' '
      obj%ncustom_xml_temp      = 0
      obj%custom_xml_temp       = ' '
      obj%ncustom_xml           = 0
      obj%custom_xml            = ' '
      obj%editor                = 'vi'
      obj%editor_temp           = obj%editor
      obj%exec_node             = 'localhost'
      call cnfg_get_value ('CFE_EXEC_NODE_DEFAULT',obj%exec_node)
      if (trim(obj%exec_node) .eq. 'localhost') then
        call getsys_env ('HOSTNAME',obj%exec_node)
      endif
      obj%exec_node_temp        = obj%exec_node

      select case (obj%process_maturity)
        case (PROCESS_PRODUCTION)
          obj%process_maturity_temp = 'PRODUCTION'
        case (PROCESS_BETA)
          obj%process_maturity_temp = 'BETA'
        case (PROCESS_ALPHA)
          obj%process_maturity_temp = 'ALPHA'
        case default
          obj%process_maturity_temp = 'RAW'
      end select

      return
      end subroutine cfemenu_initialize


!!-------------------------------- delete ----------------------------------!!
!!-------------------------------- delete ----------------------------------!!
!!-------------------------------- delete ----------------------------------!!


      subroutine cfemenu_delete (obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      call filebox_delete (obj%buildjob_dialog)
      call filebox_delete (obj%editwork_dialog)
      call filebox_delete (obj%editjob_dialog)
      call filebox_delete (obj%editfile_dialog)
      call filebox_delete (obj%viewwork_dialog)
      call filebox_delete (obj%viewjob_dialog)
      call filebox_delete (obj%viewreport_dialog)
      call filebox_delete (obj%viewfile_dialog)

      if (associated(obj%cwd_dirs))        deallocate(obj%cwd_dirs)
      if (associated(obj%custom_xml_temp)) deallocate(obj%custom_xml_temp)
      if (associated(obj%custom_xml))      deallocate(obj%custom_xml)

      return
      end subroutine cfemenu_delete


!!-------------------------------- option ----------------------------------!!
!!-------------------------------- option ----------------------------------!!
!!-------------------------------- option ----------------------------------!!


      subroutine cfemenu_option (obj,keyword)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument
      character(len=*),intent(in)         :: keyword                  !argument

      integer                             :: i                        !local
      integer                             :: j                        !local
      character(len=PC_LENGTH)            :: local_key                !local
      type(cfewindow_struct),pointer      :: window                   !local
      integer                             :: window_id                !local
      character(len=PC_LENGTH)            :: xml_name                 !local
      type(buildlist_struct),pointer      :: parameter                !local

      nullify (window) ! jpa
      nullify (parameter) ! jpa

      local_key = keyword
!     call string_to_upper (local_key)

      select case (trim(local_key))
        case ('BUILDJOBFILE')
          call cfewindow_create           (window)
          call cfewindow_set_keyword      (window ,local_key)
          call cfewindow_set_window_type  (window ,'CFE_MENU')
          call cfewindow_get_window_id    (window ,window_id)
          call cfegui_create (window_id,'Build Jobfile',  &
                               'cfe_buildjobfile.xml',.false.)
          call cfewindow_set_current (window)

        case ('EDITWORKFILE')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,local_key)
            call cfewindow_set_window_type  (window ,'CFE_MENU')
            call cfewindow_get_window_id    (window ,window_id)
            call cfegui_create (window_id,'Text Edit Workfile',  &
                                 'cfe_editworkfile.xml',.false.)
            call cfewindow_set_current (window)
            if (len_trim(obj%currentJobName) .ne. 0) then
              obj%editworkfile = trim(obj%working_dir)//trim(obj%currentJobName)
              call pc_put ('EDITWORKFILE',obj%editworkfile)
            endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('EDITJOBFILE')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,local_key)
            call cfewindow_set_window_type  (window ,'CFE_MENU')
            call cfewindow_get_window_id    (window ,window_id)
            call cfegui_create (window_id,'Text Edit Jobfile',  &
                                 'cfe_editjobfile.xml',.false.)
            call cfewindow_set_current (window)
            if (len_trim(obj%last_jobfile) .ne. 0) then
              obj%editjobfile = trim(obj%last_jobfile)
              call pc_put ('EDITJOBFILE',obj%editjobfile)
            endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('EDITFILE')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,local_key)
            call cfewindow_set_window_type  (window ,'CFE_MENU')
            call cfewindow_get_window_id    (window ,window_id)
            call cfegui_create (window_id,'Text Editor',  &
                                 'cfe_editfile.xml',.false.)
            call cfewindow_set_current (window)
          else
            call pc_error ('This option is not supported in client/server mode')
          endif
  
        case ('VIEWWORKFILE')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,local_key)
            call cfewindow_set_window_type  (window ,'CFE_MENU')
            call cfewindow_get_window_id    (window ,window_id)
            call cfegui_create (window_id,'View Workfile',  &
                                 'cfe_viewworkfile.xml',.false.)
            call cfewindow_set_current (window)
            if (len_trim(obj%currentJobName) .ne. 0) then
              obj%viewworkfile = trim(obj%working_dir)//trim(obj%currentJobName)
              call pc_put ('VIEWWORKFILE',obj%viewworkfile)
            endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('VIEWJOBFILE')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,local_key)
            call cfewindow_set_window_type  (window ,'CFE_MENU')
            call cfewindow_get_window_id    (window ,window_id)
            call cfegui_create (window_id,'View Jobfile',  &
                                 'cfe_viewjobfile.xml',.false.)
            call cfewindow_set_current (window)
            if (len_trim(obj%last_jobfile) .ne. 0) then
              obj%viewjobfile = trim(obj%last_jobfile)
              call pc_put ('VIEWJOBFILE',obj%viewjobfile)
            endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('VIEWREPORTFILE')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,local_key)
            call cfewindow_set_window_type  (window ,'CFE_MENU')
            call cfewindow_get_window_id    (window ,window_id)
            call cfegui_create (window_id,'View Reportfile',  &
                                 'cfe_viewreportfile.xml',.false.)
            call cfewindow_set_current (window)
            if (len_trim(obj%last_reportfile) .ne. 0) then
              obj%viewreportfile = trim(obj%last_reportfile)
              call pc_put ('VIEWREPORTFILE',obj%viewreportfile)
            endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('VIEWFILE')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,local_key)
            call cfewindow_set_window_type  (window ,'CFE_MENU')
            call cfewindow_get_window_id    (window ,window_id)
            call cfegui_create (window_id,'View Any Text File',  &
                                 'cfe_viewfile.xml',.false.)
            call cfewindow_set_current (window)
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('VIEWCPSHEADERS')
          call cfewindow_create           (window)
          call cfewindow_set_keyword      (window ,local_key)
          call cfewindow_set_window_type  (window ,'CFE_MENU')
          call cfewindow_get_window_id    (window ,window_id)
          call cfegui_create (window_id,'CPS Trace Header Definition',  &
                               'cps_headers.xml',.false.)
          call cfewindow_set_current (window)

        case ('CHANGEDIRECTORY')
          obj%working_dir_temp = obj%working_dir
          call cfewindow_create           (window)
          call cfewindow_set_keyword      (window ,local_key)
          call cfewindow_set_window_type  (window ,'CFE_MENU')
          call cfewindow_get_window_id    (window ,window_id)
          call cfegui_create (window_id,'Change Working Directory',  &
                               'cfe_changedirectory.xml',.false.)
          call cfewindow_set_current (window)

        case ('TOOLBAREDITOR')

        case ('USERPREFERENCES')
          call cfewindow_create           (window)
          call cfewindow_set_keyword      (window ,local_key)
          call cfewindow_set_window_type  (window ,'CFE_MENU')
          call cfewindow_get_window_id    (window ,window_id)
          call cfegui_create (window_id,'User Preferences',  &
                               'cfe_userpreferences.xml',.false.)
          call cfewindow_set_current (window)

        case ('PROGPREFERENCES')
          if (associated(obj%custom_xml_temp)) deallocate(obj%custom_xml_temp)
          obj%ncustom_xml_temp = obj%ncustom_xml
          if (obj%ncustom_xml_temp .gt. 0) then
            allocate(obj%custom_xml_temp(obj%ncustom_xml_temp))
            obj%custom_xml_temp = obj%custom_xml
          else
            allocate(obj%custom_xml_temp(1))
            obj%custom_xml_temp = ' '
          endif
          call cfewindow_create           (window)
          call cfewindow_set_keyword      (window ,local_key)
          call cfewindow_set_window_type  (window ,'CFE_MENU')
          call cfewindow_get_window_id    (window ,window_id)
          xml_name = 'cfe_progpreferences.xml'
          if (obj%ncustom_xml .gt. 0) then
            do i=1,obj%ncustom_xml
!!!           j = index        (obj%custom_xml(i),trim(xml_name),.true.)
              j = string_index (obj%custom_xml(i),trim(xml_name),.true.)
              if (j .gt. 0) then
                xml_name = obj%custom_xml(i)
                exit
              endif
            enddo
          endif
          call cfegui_create (window_id,'Programmer Preferences',  &
                               trim(xml_name),.false.)
          call cfewindow_set_current (window)

        case ('BUILDLIST')
          window_id=cfewindow_match ('PARAMETER','BUILDLIST','BUILDLIST',0)
          if (window_id .ne. -1) then
            call cfegui_jump_window (window_id)
            return
          endif
          call buildlist_create (parameter,obj%working_dir,2,'BUILDLIST',' ',  &
                                 .false.)
          call buildlist_set_buffer (parameter,object%masterBuffer)
          call pc_put_gui ('RUNTRAPS'     ,'VISIBLE','FALSE')
          call pc_put_gui ('PARAMETERLIST','VISIBLE','FALSE')
          call cfewindow_create          (window)
          call cfewindow_set_window_type (window ,'PARAMETER')
          call cfewindow_set_keyword     (window ,'BUILDLIST')
          call cfewindow_set_value       (window ,'BUILDLIST')
          call cfewindow_set_parameter   (window ,parameter)
          call cfewindow_get_window_id   (window ,window_id)

          call cfegui_create (window_id,'Build List','buildlist.xml',.false.)

          call cfewindow_set_current (window)

        case ('APPLICATIONVERSION')
          call cfegui_clientversion()
        case ('DOC')
      end select

      return
      end subroutine cfemenu_option


!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!


      subroutine cfemenu_update (obj,keyword)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument
      character(len=*),intent(in)         ::  keyword                 !argument

      character(len=PC_LENGTH)            :: local_key                !local

      character(len=PC_LENGTH)            :: filter                   !local
      type(cfewindow_struct),pointer      :: window                   !local
      integer                             :: window_id                !local
      integer                             :: j1                       !local
      integer                             :: j2                       !local
      integer                             :: indx                     !local
      integer,pointer                     :: itmp_array(:)            !local
      character(len=10)                   :: process_maturity_menu(4) !local
      integer                             :: nprocess_maturity_menu   !local
      character(len=8)                    :: editor_menu(3)           !local
      integer                             :: neditor_menu             !local

      nullify (window) ! jpa

      local_key = keyword
!     call string_to_upper (local_key)

      select case (trim(local_key))
        case ('BUILDJOBFILE')
          if (pc_gui_action_present('SELECTFILE','ButtonPress')) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,'BUILDJOBFILE')
            call cfewindow_set_window_type  (window ,'FILEBOX')
            call cfewindow_get_window_id    (window ,window_id)

            filter = trim(obj%working_dir) // '*.wrk'
            if (.not. associated(obj%buildjob_dialog)) then
              call filebox_create (obj%buildjob_dialog,filter)
            else
              call filebox_update (obj%buildjob_dialog)
            endif

            call cfegui_create (window_id,'FILE SELECTION','filebox.xml',  &
                                 .false.)
            call cfewindow_set_current (window)
          endif

        case ('EDITWORKFILE')
          if (pc_gui_action_present('SELECTFILE','ButtonPress')) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,'EDITWORKFILE')
            call cfewindow_set_window_type  (window ,'FILEBOX')
            call cfewindow_get_window_id    (window ,window_id)

            filter = trim(obj%working_dir) // '*.wrk'
            if (.not. associated(obj%editwork_dialog)) then
              call filebox_create (obj%editwork_dialog,filter)
            else
              call filebox_update (obj%editwork_dialog)
            endif

            call cfegui_create (window_id,'FILE SELECTION','filebox.xml',  &
                                 .false.)
            call cfewindow_set_current (window)
          endif

        case ('EDITJOBFILE')
          if (pc_gui_action_present('SELECTFILE','ButtonPress')) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,'EDITJOBFILE')
            call cfewindow_set_window_type  (window ,'FILEBOX')
            call cfewindow_get_window_id    (window ,window_id)

            filter = trim(obj%working_dir) // '*.job'
            if (.not. associated(obj%editjob_dialog)) then
              call filebox_create (obj%editjob_dialog,filter)
            else
              call filebox_update (obj%editjob_dialog)
            endif

            call cfegui_create (window_id,'FILE SELECTION','filebox.xml',  &
                                 .false.)
            call cfewindow_set_current (window)
          endif

        case ('EDITFILE')
          if (pc_gui_action_present('SELECTFILE','ButtonPress')) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,'EDITFILE')
            call cfewindow_set_window_type  (window ,'FILEBOX')
            call cfewindow_get_window_id    (window ,window_id)

            filter = trim(obj%working_dir) // '*'
            if (.not. associated(obj%editfile_dialog)) then
              call filebox_create (obj%editfile_dialog,filter)
            else
              call filebox_update (obj%editfile_dialog)
            endif

            call cfegui_create (window_id,'FILE SELECTION','filebox.xml',  &
                                 .false.)
            call cfewindow_set_current (window)
          endif

        case ('VIEWWORKFILE')
          if (pc_gui_action_present('SELECTFILE','ButtonPress')) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,'VIEWWORKFILE')
            call cfewindow_set_window_type  (window ,'FILEBOX')
            call cfewindow_get_window_id    (window ,window_id)

            filter = trim(obj%working_dir) // '*.wrk'
            if (.not. associated(obj%viewwork_dialog)) then
              call filebox_create (obj%viewwork_dialog,filter)
            else
              call filebox_update (obj%viewwork_dialog)
            endif

            call cfegui_create (window_id,'FILE SELECTION','filebox.xml',  &
                                 .false.)
            call cfewindow_set_current (window)
          endif

        case ('VIEWJOBFILE')
          if (pc_gui_action_present('SELECTFILE','ButtonPress')) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,'VIEWJOBFILE')
            call cfewindow_set_window_type  (window ,'FILEBOX')
            call cfewindow_get_window_id    (window ,window_id)

            filter = trim(obj%working_dir) // '*.job'
            if (.not. associated(obj%viewjob_dialog)) then
              call filebox_create (obj%viewjob_dialog,filter)
            else
              call filebox_update (obj%viewjob_dialog)
            endif

            call cfegui_create (window_id,'FILE SELECTION','filebox.xml',  &
                                 .false.)
            call cfewindow_set_current (window)
          endif

        case ('VIEWREPORTFILE')
          if (pc_gui_action_present('SELECTFILE','ButtonPress')) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,'VIEWREPORTFILE')
            call cfewindow_set_window_type  (window ,'FILEBOX')
            call cfewindow_get_window_id    (window ,window_id)

            filter = trim(obj%working_dir) // '*.rpt.*'
            if (.not. associated(obj%viewreport_dialog)) then
              call filebox_create (obj%viewreport_dialog,filter)
            else
              call filebox_update (obj%viewreport_dialog)
            endif

            call cfegui_create (window_id,'FILE SELECTION','filebox.xml',  &
                                 .false.)
            call cfewindow_set_current (window)
          endif

        case ('VIEWFILE')
          if (pc_gui_action_present('SELECTFILE','ButtonPress')) then
            call cfewindow_create           (window)
            call cfewindow_set_keyword      (window ,'VIEWFILE')
            call cfewindow_set_window_type  (window ,'FILEBOX')
            call cfewindow_get_window_id    (window ,window_id)

            filter = trim(obj%working_dir) // '*'
            if (.not. associated(obj%viewfile_dialog)) then
              call filebox_create (obj%viewfile_dialog,filter)
            else
              call filebox_update (obj%viewfile_dialog)
            endif

            call cfegui_create (window_id,'FILE SELECTION','filebox.xml',  &
                                 .false.)
            call cfewindow_set_current (window)
          endif

        case ('CHANGEDIRECTORY')
          call pc_get ('WORKING_DIR', obj%working_dir_temp)
          if (pc_gui_action_present('DIRECTORIES','ItemSelected')) then
            nullify(itmp_array)
            call pc_alloc_gui ('DIRECTORIES','ItemSelected',itmp_array,indx)
            if (indx .eq. 1) then
              indx = itmp_array(1)
              if (associated(itmp_array)) deallocate(itmp_array)
            else if (indx .gt. 1) then
              call pc_error ('Only one element may be selected')
              if (associated(itmp_array)) deallocate(itmp_array)
              return
            else
              if (associated(itmp_array)) deallocate(itmp_array)
              return
            endif
            if (trim(obj%cwd_dirs(indx)) .eq. '.') then
              return
            else if (trim(obj%cwd_dirs(indx)) .eq. '..') then
              j1 = len_trim(obj%working_dir_temp)
!!!           j2 = index        (trim(obj%working_dir_temp),'/',.true.)
              j2 = string_index (trim(obj%working_dir_temp),'/',.true.)
              if (j1.eq.j2 .and. j2.gt.0) then
!!!             j2 = index        (obj%working_dir_temp(1:j1-1),'/',.true.)
                j2 = string_index (obj%working_dir_temp(1:j1-1),'/',.true.)
                if (j2 .gt. 0) then
                  obj%working_dir_temp = trim(obj%working_dir_temp(1:j2))
                else
                  return
                endif
              else
                return
              endif
            else
              obj%working_dir_temp = trim(obj%working_dir_temp) //  & 
                                     trim(obj%cwd_dirs(indx))
            endif
          endif
          j1 = len_trim(obj%working_dir_temp)
!!!       j2 = index        (trim(obj%working_dir_temp),'/',.true.)
          j2 = string_index (trim(obj%working_dir_temp),'/',.true.)
          if (j1 .ne. j2) obj%working_dir_temp(j1+1:j1+1) = '/'
          call cfemenu_load_cwd_dirs (obj)

          call pc_put_minsize_array ('DIRECTORIES',obj%ncwd_dirs)
          call pc_put_maxsize_array ('DIRECTORIES',obj%ncwd_dirs)

          call pc_put ('WORKING_DIR', obj%working_dir_temp)
          call pc_put ('DIRECTORIES', obj%cwd_dirs        , obj%ncwd_dirs)

        case ('TOOLBAREDITOR')

        case ('USERPREFERENCES')
          call pc_get ('EDITOR',obj%editor_temp)
          neditor_menu   = 3
          editor_menu(1) = 'vi'
          editor_menu(2) = 'nedit'
          editor_menu(3) = 'xemacs'
          call pc_put_options_field ('EDITOR',editor_menu,neditor_menu)
          call pc_put ('EDITOR',obj%editor_temp)

          call pc_get ('EXEC_NODE',obj%exec_node_temp)
          call pc_put ('EXEC_NODE',obj%exec_node_temp)

        case ('PROGPREFERENCES')
          call pc_get   ('MATURITY'  ,obj%process_maturity_temp)
          call pc_alloc ('CUSTOM_XML',obj%custom_xml_temp,obj%ncustom_xml_temp)

          process_maturity_menu(1) = 'PRODUCTION'
          process_maturity_menu(2) = 'BETA'
          process_maturity_menu(3) = 'ALPHA'
          process_maturity_menu(4) = 'RAW'
          nprocess_maturity_menu   = 4
          call pc_put_options_field ('MATURITY',process_maturity_menu,  &
                                                nprocess_maturity_menu)

          call pc_put   ('MATURITY'  ,obj%process_maturity_temp)
          call pc_put   ('CUSTOM_XML',obj%custom_xml_temp,obj%ncustom_xml_temp)

        case ('APPLICATIONVERSION')
        case ('DOC')
      end select

      return
      end subroutine cfemenu_update


!!------------------------------- execute ----------------------------------!!
!!------------------------------- execute ----------------------------------!!
!!------------------------------- execute ----------------------------------!!


      subroutine cfemenu_execute (obj,keyword,err)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument
      character(len=*),intent(in)         :: keyword                  !argument
      logical         ,intent(out)        :: err                      !argument

      character(len=PC_LENGTH)            :: local_key                !local


      character(len=PC_LENGTH)            :: msg                      !local
      character(len=FILENAME_LENGTH)      :: cpsdoc_file              !local
      character(len=PC_LENGTH),pointer    :: cards(:)                 !local
      character(len=PC_LENGTH)            :: node                     !local
      character(len=PC_LENGTH)            :: cpsdoc_node              !local
      type(buildlist_struct),pointer      :: parameter                !local
      type(cfewindow_struct),pointer      :: window                   !local
      integer                             :: i                        !local
      integer                             :: j                        !local
      integer                             :: ncards                   !local
      integer                             :: window_id                !local
      integer                             :: istat                    !local

      nullify(window)
      nullify(parameter)
      nullify(cards)

      ncards = 0
      err = .false.
      local_key = keyword
!     call string_to_upper (local_key)

      select case (trim(local_key))
        case ('BUILDJOBFILE')
          write(STDOUT,*) 'cfebld ' // trim(obj%buildjobfile)
          call pc_info ('Built jobfile for ' // trim(obj%buildjobfile))

        case ('EDITWORKFILE')
          istat = finquire_output(obj%editworkfile)
          if (istat .eq. FINQUIRE_FOUND) then
            if (trim(obj%editor) .eq. 'vi') then
              call getsys_hostname (node)
              write(STDOUT,*) 'xon '//trim(node)//  &
                              ' "xterm -T '//trim(obj%editworkfile)//  &
                              ' -e vi '//trim(obj%editworkfile)//'"'
              call putsys_texec ('xon '//trim(node)//  &
                                 ' "xterm -T '//trim(obj%editworkfile)//  &
                                 ' -e vi '//trim(obj%editworkfile)//'"')
            else if (trim(obj%editor) .eq. 'xemacs') then
              write(STDOUT,*) 'xemacs -T '//trim(obj%editworkfile)//  &
                              ' '//trim(obj%editworkfile)//' &'
              call putsys_texec ('xemacs -T '//trim(obj%editworkfile)//  &
                                 ' '//trim(obj%editworkfile)//' &')
            else if (trim(obj%editor) .eq. 'nedit') then
              write(STDOUT,*) 'nedit '//trim(obj%editworkfile)
              call putsys_texec ('nedit '//trim(obj%editworkfile)//' &')
            endif
          else
            call pc_error ('Can not open file '//trim(obj%editworkfile)//  &
                           ' for write.')
          endif

        case ('EDITJOBFILE')
          istat = finquire_output(obj%editjobfile)
          if (istat .eq. FINQUIRE_FOUND) then
            if (trim(obj%editor) .eq. 'vi') then
              call getsys_hostname (node)
              write(STDOUT,*) 'xon '//trim(node)//  &
                              ' "xterm -T '//trim(obj%editjobfile)//  &
                              ' -e vi '//trim(obj%editjobfile)//'"'
              call putsys_texec ('xon '//trim(node)//  &
                                 ' "xterm -T '//trim(obj%editjobfile)//  &
                                 ' -e vi '//trim(obj%editjobfile)//'"')
            else if (trim(obj%editor) .eq. 'xemacs') then
              write(STDOUT,*) 'xemacs -T '//trim(obj%editjobfile)//  &
                              ' '//trim(obj%editjobfile)//' &'
              call putsys_texec ('xemacs -T '//trim(obj%editjobfile)//  &
                                 ' '//trim(obj%editjobfile)//' &')
            else if (trim(obj%editor) .eq. 'nedit') then
              write(STDOUT,*) 'nedit '//trim(obj%editjobfile)
              call putsys_texec ('nedit '//trim(obj%editjobfile)//' &')
            endif
          else
            call pc_error ('Can not open file '//trim(obj%editjobfile)//  &
                           ' for write.')
          endif

        case ('EDITFILE')
          istat = finquire_output(obj%editfile)
          if (istat .eq. FINQUIRE_FOUND) then
            if (trim(obj%editor) .eq. 'vi') then
              call getsys_hostname (node)
              write(STDOUT,*) 'xon '//trim(node)//  &
                              ' "xterm -T '//trim(obj%editfile)//  &
                              ' -e vi '//trim(obj%editfile)//'"'
              call putsys_texec ('xon '//trim(node)//  &
                                 ' "xterm -T '//trim(obj%editfile)//  &
                                 ' -e vi '//trim(obj%editfile)//'"')
            else if (trim(obj%editor) .eq. 'xemacs') then
              write(STDOUT,*) 'xemacs -T '//trim(obj%editfile)//  &
                              ' '//trim(obj%editfile)//' &'
              call putsys_texec ('xemacs -T '//trim(obj%editfile)//  &
                                 ' '//trim(obj%editfile)//' &')
            else if (trim(obj%editor) .eq. 'nedit') then
              write(STDOUT,*) 'nedit '//trim(obj%editfile)
              call putsys_texec ('nedit '//trim(obj%editfile)//' &')
            endif
          else
            call pc_error ('Can not open file '//trim(obj%editfile)//  &
                           ' for write.')
          endif

        case ('VIEWWORKFILE')
          istat = finquire_input(obj%viewworkfile)
          if (istat .eq. FINQUIRE_FOUND) then
            if (trim(obj%editor) .eq. 'vi') then
              call getsys_hostname (node)
              write(STDOUT,*) 'xon '//trim(node)//  &
                              ' "xterm -T '//trim(obj%viewworkfile)//  &
                              ' -e view '//trim(obj%viewworkfile)//'"'
              call putsys_texec ('xon '//trim(node)//  &
                                 ' "xterm -T '//trim(obj%viewworkfile)//  &
                                 ' -e view '//trim(obj%viewworkfile)//'"')
            else if (trim(obj%editor) .eq. 'xemacs') then
              call pc_error ('This option is not supported with xemacs')
            else if (trim(obj%editor) .eq. 'nedit') then
              write(STDOUT,*) 'nedit -read '//trim(obj%viewworkfile)
              call putsys_texec ('nedit -read '//trim(obj%viewworkfile)//' &')
            endif
           else
            call pc_error ('File '//trim(obj%viewworkfile)//  &
                           ' not found.')
          endif

        case ('VIEWJOBFILE')
          istat = finquire_input(obj%viewjobfile)
          if (istat .eq. FINQUIRE_FOUND) then
            if (trim(obj%editor) .eq. 'vi') then
              call getsys_hostname (node)
              write(STDOUT,*) 'xon '//trim(node)//  &
                              ' "xterm -T '//trim(obj%viewjobfile)//  &
                              ' -e view '//trim(obj%viewjobfile)//'"'
              call putsys_texec ('xon '//trim(node)//  &
                                 ' "xterm -T '//trim(obj%viewjobfile)//  &
                                 ' -e view '//trim(obj%viewjobfile)//'"')
            else if (trim(obj%editor) .eq. 'xemacs') then
              call pc_error ('This option is not supported with xemacs')
            else if (trim(obj%editor) .eq. 'nedit') then
              write(STDOUT,*) 'nedit -read '//trim(obj%viewjobfile)
              call putsys_texec ('nedit -read '//trim(obj%viewjobfile)//' &')
            endif
           else
            call pc_error ('File '//trim(obj%viewjobfile)//  &
                           ' not found.')
          endif

        case ('VIEWREPORTFILE')
          istat = finquire_input(obj%viewreportfile)
          if (istat .eq. FINQUIRE_FOUND) then
            if (trim(obj%editor) .eq. 'vi') then
              call getsys_hostname (node)
              write(STDOUT,*) 'xon '//trim(node)//  &
                              ' "xterm -T '//trim(obj%viewreportfile)//  &
                              ' -e view '//trim(obj%viewreportfile)//'"'
              call putsys_texec ('xon '//trim(node)//  &
                                 ' "xterm -T '//trim(obj%viewreportfile)//  &
                                 ' -e view '//trim(obj%viewreportfile)//'"')
            else if (trim(obj%editor) .eq. 'xemacs') then
              call pc_error ('This option is not supported with xemacs')
            else if (trim(obj%editor) .eq. 'nedit') then
              write(STDOUT,*) 'nedit -read '//trim(obj%viewreportfile)
              call putsys_texec ('nedit -read '//trim(obj%viewreportfile)//' &')
            endif
           else
            call pc_error ('File '//trim(obj%viewreportfile)//  &
                           ' not found.')
          endif

        case ('VIEWFILE')
          istat = finquire_input(obj%viewfile)
          if (istat .eq. FINQUIRE_FOUND) then
            if (trim(obj%editor) .eq. 'vi') then
              call getsys_hostname (node)
              write(STDOUT,*) 'xon '//trim(node)//  &
                              ' "xterm -T '//trim(obj%viewfile)//  &
                              ' -e vi '//trim(obj%viewfile)//'"'
              call putsys_texec ('xon '//trim(node)//  &
                                 ' "xterm -T '//trim(obj%viewfile)//  &
                                 ' -e view '//trim(obj%viewfile)//'"')
            else if (trim(obj%editor) .eq. 'xemacs') then
              call pc_error ('This option is not supported with xemacs')
            else if (trim(obj%editor) .eq. 'nedit') then
              write(STDOUT,*) 'nedit -read '//trim(obj%viewfile)
              call putsys_texec ('nedit -read '//trim(obj%viewfile)//' &')
            endif
           else
            call pc_error ('File '//trim(obj%viewfile)//  &
                           ' not found.')
          endif

        case ('CHANGEDIRECTORY')
          call cfeutil_inquire_directory (obj%working_dir_temp,istat,msg)
          if (istat .ne. CFE_NAME_VALID) then
            err = .true.
            call pc_error (msg)
          else
            istat = finquire_output(trim(obj%working_dir_temp)//'.xyz')
            if (istat .eq. FINQUIRE_ERROR) then
              err = .true.
              call pc_error ('Can not create files in this directory')
            else
              obj%working_dir = obj%working_dir_temp
              if (associated(obj%buildjob_dialog)) then
                call filebox_change_filter (obj%buildjob_dialog,  &
                                            trim(obj%working_dir)//'*.wrk')
              endif
              if (associated(obj%editwork_dialog)) then
                call filebox_change_filter (obj%editwork_dialog,  &
                                            trim(obj%working_dir)//'*.wrk')
              endif
              if (associated(obj%editjob_dialog)) then
                call filebox_change_filter (obj%editjob_dialog,  &
                                            trim(obj%working_dir)//'*.job')
              endif
              if (associated(obj%editfile_dialog)) then
                call filebox_change_filter (obj%editfile_dialog,  &
                                            trim(obj%working_dir)//'*')
              endif
              if (associated(obj%viewwork_dialog)) then
                call filebox_change_filter (obj%viewwork_dialog,  &
                                            trim(obj%working_dir)//'*.wrk')
              endif
              if (associated(obj%viewjob_dialog)) then
                call filebox_change_filter (obj%viewjob_dialog,  &
                                            trim(obj%working_dir)//'*.job')
              endif
              if (associated(obj%viewreport_dialog)) then
                call filebox_change_filter (obj%viewreport_dialog,  &
                                            trim(obj%working_dir)//'*.rpt')
              endif
              if (associated(obj%viewfile_dialog)) then
                call filebox_change_filter (obj%viewfile_dialog,  &
                                            trim(obj%working_dir)//'*')
              endif
              if (associated(obj%selectCurrentDialog)) then
                call filebox_change_filter (obj%selectCurrentDialog,  &
                                            trim(obj%working_dir)//'*.wrk')
              endif
              if (associated(obj%selectJobDialog)) then
                call filebox_change_filter (obj%selectJobDialog,  &
                                            trim(obj%working_dir)//'*.wrk')
              endif
              if (associated(obj%SJjobs_dialog)) then
                call mfilebox_change_filter (obj%SJjobs_dialog,  &
                                            trim(obj%working_dir)//'*.job')
              endif
              if (associated(obj%templateJobDialog)) then
                call filebox_change_filter (obj%templateJobDialog,  &
                                            trim(obj%working_dir)//'*.wrk')
              endif
              call getsys_hostname (node)
              call pc_put_gui_only ('WORKINGDIR','Working Environment = '//  &
                                    trim(obj%userid)//'@'//trim(node)//':'//  &
                                    trim(obj%working_dir))
              if (associated(obj%currentWorkfile)) then
                call cfewb_clear_current_workfile (obj%currentJobName,     &
                                                   obj%currentProcessList, &
                                                   obj%NcurrentProcessList,&
                                                   obj%currentWorkfile)
              endif
              call cfemwb_clear(obj)
              window_id=cfewindow_match ('PARAMETER','BUILDLIST','BUILDLIST',0)
              if (window_id .ne. -1) then
                call cfewindow_get_pointer   (window_id,window )
                call cfewindow_get_parameter (window   ,parameter)
                call buildlist_change_dir    (parameter,trim(obj%working_dir))
              endif
            endif
          endif
        case ('TOOLBAREDITOR')

        case ('USERPREFERENCES')
          obj%editor    = obj%editor_temp
          obj%exec_node = obj%exec_node_temp
          call cfeutil_save_session (obj)

        case ('PROGPREFERENCES')
          i = obj%process_maturity
          select case (trim(obj%process_maturity_temp))
            case ('PRODUCTION')
              obj%process_maturity = PROCESS_PRODUCTION
            case ('BETA')
              obj%process_maturity = PROCESS_BETA
            case ('ALPHA')
              obj%process_maturity = PROCESS_ALPHA
            case default
              obj%process_maturity = PROCESS_RAW
          end select
          if (i .ne. obj%process_maturity) then
            call process_set_view   (obj%process_maturity)
            call cfewb_subset_list (obj%lnklib,obj%processSubset,  &
                                   obj%subsetProcessList,obj%NsubsetProcessList)
          endif

          if (associated(obj%custom_xml)) deallocate(obj%custom_xml)
          obj%ncustom_xml = obj%ncustom_xml_temp
          if (obj%ncustom_xml .gt. 0) then
            allocate(obj%custom_xml(obj%ncustom_xml))
            obj%custom_xml = obj%custom_xml_temp
          else
            allocate(obj%custom_xml(1))
            obj%custom_xml = ' '
          endif
          call cfeutil_save_session (obj)

        case ('CIU')

          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            write(STDOUT,*) 'sh -c ciu -l 2>&1 &'
            call putsys_texec ('sh -c ciu -l &')
            !call getsys_hostname (node)
            !write(STDOUT,*) 'xon '//trim(node)//' ciu -l'
            !call putsys_texec ('xon '//trim(node)//' ciu -l')
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('XPBS')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            write(STDOUT,*) 'sh -c xpbs 2>&1 &'
            call putsys_texec ('sh -c xpbs 2>&1 &')
            !if (len_trim(obj%exec_node) .gt. 0) then
            !  write(STDOUT,*) 'xon '//trim(obj%exec_node)//' xpbs'
            !  call putsys_texec ('xon '//trim(obj%exec_node)//' xpbs')
            !else
            !  call pc_error ('Execution node not set in user preferences')
            !endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('XTPIOADMIN')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            write(STDOUT,*) 'sh -c Xtpioadmin 2>&1 &'
            call putsys_texec ('sh -c Xtpioadmin 2>&1 &')
            !if (len_trim(obj%exec_node) .gt. 0) then
            ! write(STDOUT,*) 'xon '//trim(obj%exec_node)//' Xtpioadmin'
            !  call putsys_texec ('xon '//trim(obj%exec_node)//' Xtpioadmin')
            !else
            !  call pc_error ('Execution node not set in user preferences')
            !endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('CBYT')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            write(STDOUT,*) 'sh -c cbyt 2>&1 &'
            call putsys_texec ('sh -c cbyt 2>&1 &')
            !if (len_trim(obj%exec_node) .gt. 0) then
            !  write(STDOUT,*) 'xon '//trim(obj%exec_node)//' cbyt'
            !  call putsys_texec ('xon '//trim(obj%exec_node)//' cbyt')
            !else
            !  call pc_error ('Execution node not set in user preferences')
            !endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('VA')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            write(STDOUT,*) 'sh -c va 2>&1 &'
            call putsys_texec ('sh -c va 2>&1 &')
            !if (len_trim(obj%exec_node) .gt. 0) then
            !  write(STDOUT,*) 'xon '//trim(obj%exec_node)//' va'
            !  call putsys_texec ('xon '//trim(obj%exec_node)//' va')
            !else
            !  call pc_error ('Execution node not set in user preferences')
            !endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('CSV')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            write(STDOUT,*) 'sh -c csv 2>&1 &'
            call putsys_texec ('sh -c csv 2>&1 &')
            !if (len_trim(obj%exec_node) .gt. 0) then
            !  write(STDOUT,*) 'xon '//trim(obj%exec_node)//' csv'
            !  call putsys_texec ('xon '//trim(obj%exec_node)//' csv')
            !else
            !  call pc_error ('Execution node not set in user preferences')
            !endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('CFG')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            write(STDOUT,*) 'sh -c cfg 2>&1 &'
            call putsys_texec ('sh -c cfg 2>&1 &')
            !if (len_trim(obj%exec_node) .gt. 0) then
            !  write(STDOUT,*) 'xon '//trim(obj%exec_node)//' cfg'
            !  call putsys_texec ('xon '//trim(obj%exec_node)//' cfg')
            !else
            !  call pc_error ('Execution node not set in user preferences')
            !endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('JOBMON')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            write(STDOUT,*) 'sh -c jobmon 2>&1 &'
            call putsys_texec ('sh -c jobmon 2>&1 &')
            !if (len_trim(obj%exec_node) .gt. 0) then
            !  write(STDOUT,*) 'xon '//trim(obj%exec_node)//' jobmon'
            !  call putsys_texec ('xon '//trim(obj%exec_node)//' jobmon')
            !else
            !  call pc_error ('Execution node not set in user preferences')
            !endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

        case ('APPLICATIONVERSION')

        case ('DOC')
          if (obj%exemode .eq. CFEJAVA_DIRECTAPP) then
            cpsdoc_file = ''
            cpsdoc_node = ''
            call cnfg_get_value ('CPSDOC_NODES_FILE',cpsdoc_file)
            if (len_trim(cpsdoc_file) .gt. 0) then
              call cfeutil_read_file (cpsdoc_file,cards,ncards)
              if (ncards .gt. 0) then
                call getsys_hostname (node)
                do i=1,ncards
                  if (cards(i)(1:1) .eq. '#') cycle
                  if (len_trim(cpsdoc_node) .eq. 0) cpsdoc_node = cards(i)
                  j = index(cards(i),'.')
                  if (j .eq. 0) then
                    if (trim(node) .eq. trim(cards(i))) then
                      cpsdoc_node = cards(i)
                      exit
                    endif 
                  else if (j .gt. 1) then
                    if (trim(node) .eq. cards(i)(1:j-1)) then
                      cpsdoc_node = cards(i)
                      exit
                    endif
                  endif
                enddo
              endif
            endif
            if (len_trim(cpsdoc_node) .gt. 0) then
!             write(STDOUT,*) 'netscape http://'//trim(cpsdoc_node)//  &
!                             '/cgi-sps/cps_doc.pl'
!             call putsys_texec ('netscape http://'//trim(cpsdoc_node)//  &
!                                '/cgi-sps/cps_doc.pl &',istat)
              write(STDOUT,*) 'seamonkey file://${CPSEIS_INSTALL_DIR}/cgi-bin/cps_doc.pl 2>&1 &'
              call putsys_texec ('seamonkey file://${CPSEIS_INSTALL_DIR}/cgi-bin/cps_doc.pl &',istat)
              write(STDOUT,'(A,I2)') 'Status code from running documentation system=',istat

            else
              call pc_error ('Seamonkey not found.  Can not display DOC')
            endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif
      end select

      return
      end subroutine cfemenu_execute


!!--------------------------- load_cwd_dirs -------------------------------!!
!!--------------------------- load_cwd_dirs -------------------------------!!
!!--------------------------- load_cwd_dirs -------------------------------!!


      subroutine cfemenu_load_cwd_dirs(obj)
      implicit none
      type(cfestruct),pointer             :: obj                      !argument

      character(len=PC_LENGTH)            :: ctemp                    !local
      integer,allocatable                 :: htemp(:)                 !local
      integer                             :: nhtemp                   !local
      integer                             :: i                        !local
      integer                             :: i1                       !local
      integer                             :: i2                       !local
      integer                             :: j                        !local
      integer                             :: j1                       !local
      integer                             :: j2                       !local
      integer                             :: cpi                      !local
      integer                             :: itype                    !local
      integer,external                    :: filebox_opendir          !local
      integer,external                    :: filebox_filter           !local
      integer,external                    :: filebox_readdir          !local
      integer,external                    :: filebox_closedir         !local
      integer,external                    :: filebox_sort             !local

      cpi = string_chars_per_integer()
      nhtemp = (PC_LENGTH+1)/cpi + 1
      allocate(htemp(nhtemp))
      htemp = 0

      j = len_trim(obj%working_dir_temp)
      if (obj%working_dir_temp(j:j) .eq. '/') then
        ctemp = obj%working_dir_temp(1:j-1)
      else
        ctemp = obj%working_dir_temp
      endif
      call string_cc2hh(trim(ctemp),htemp)
      i = filebox_opendir(htemp)
      if (i .ne. 0) then
        call pc_error ('Invalid Directory')
        deallocate(htemp)
        return                         !Error opening directory
      endif

      if (associated(obj%cwd_dirs))  deallocate(obj%cwd_dirs)
      obj%ncwd_dirs  = 0

      do
        i = filebox_readdir(htemp,itype)
        if (htemp(1) .eq. 0) exit
        call string_hh2cc(htemp,ctemp)
        if (itype .eq. 1) then             !directory
          if (object%ncwd_dirs .eq. 0) then
            allocate(obj%cwd_dirs(1))
            obj%cwd_dirs(1) = trim(ctemp)
            obj%ncwd_dirs = 1
          else
            call cfeutil_append_array_element (obj%cwd_dirs,obj%ncwd_dirs,  &
                                               trim(ctemp))
          endif
        endif
      enddo
      deallocate(htemp)
      i = filebox_closedir()

      i1 = nhtemp*cpi
      i2 = object%ncwd_dirs
      allocate(htemp(nhtemp*i2))
      do i=1,i2
        j1 = 1 + (i-1)*nhtemp
        j2 = j1 + nhtemp - 1
        call string_cc2hh (object%cwd_dirs(i),htemp(j1:j2))
      enddo
      i = filebox_sort (htemp, i1, i2)
      do i=1,i2
        j1 = 1 + (i-1)*nhtemp
        j2 = j1 + nhtemp - 1
        call string_hh2cc (htemp(j1:j2),object%cwd_dirs(i))
      enddo
      deallocate(htemp)


      return
      end subroutine cfemenu_load_cwd_dirs


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module cfemenu_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

