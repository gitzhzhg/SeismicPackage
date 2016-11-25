!<CPS_v1 type="PROGRAM"/>
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
!                       C P S   P R O G R A M
!
! Name       : ciu
! Category   : stand-alone
! Written    : 2002-04-23   by: Donna K. Vunderink
! Revised    : 2002-10-09   by: Donna K. Vunderink
! Maturity   : production
! Purpose    : CPS Interactive Utilities
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!
!-------------------------------------------------------------------------------
!</descript_doc>


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
!  4. 2002-10-09  Vunderink    Modified to use new prog_module layer and
!                                added support for defaults.
!  3. 2002-10-03  Vunderink    Removed Ponca node dependencies in starting
!                                cpsDOC.
!  2. 2002-04-25  Vunderink    Changed title, removed development code and
!                                added CIU_WORKING_DIR global
!  1. 2002-04-23  Vunderink    Initial version.
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
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="WorkingDir">
!<Tip> The node, userid and working directory for this utils session. </Tip>
! This is a view-only field.
!</Help>
!
!</HelpSection>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module ciu_module

      use cio_module
      use ciu_gui_module
      use ciu_window_module
      use cnfg_module
      use finquire_module
      use getsys_module
      use prog_module
      use pc_module
      use pclun_module
      use putsys_module

      implicit none

      private
      public :: ciu_create
      public :: ciu_initialize
      public :: ciu_update
      public :: ciu_delete

      character(len=100),public,save :: ciu_ident = &
       '$Id: ciu.f90,v 1.1 2008/02/15 17:05:48 mengewm Exp $'

      integer,parameter,private   :: STDOUT               =  6

      integer,parameter,public    :: CIU_UNKNOWN          =  0
      integer,parameter,public    :: CIU_PRODLIB          =  1
      integer,parameter,public    :: CIU_BETALIB          =  2
      integer,parameter,public    :: CIU_ALPHALIB         =  3

      integer,parameter,public    :: CIU_DIRECTAPP        =  1
      integer,parameter,public    :: CIU_CSAPP            =  2

      integer,parameter,public    :: CIU_NAME_INVALID     = -1
      integer,parameter,public    :: CIU_NAME_VALID       =  1


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: ciu_struct

        integer                                      :: stdout
        integer                                      :: lnklib
        integer                                      :: exemode
        integer                                      :: mainWindowID
        integer                                      :: ScreenId
        character(len=12)                            :: userid
        character(len=24)                            :: defaults
        integer                                      :: NProgList
        character(len=PC_LENGTH),pointer             :: ProgList(:)
        character(len=PC_LENGTH)                     :: working_dir
        character(len=PC_LENGTH)                     :: working_dir_temp
        character(len=PC_LENGTH),pointer             :: cwd_dirs(:)
        integer                                      :: ncwd_dirs
        character(len=PC_LENGTH) ,pointer            :: custom_xml_temp(:)
        integer                                      :: ncustom_xml_temp
        character(len=PC_LENGTH) ,pointer            :: custom_xml(:)
        integer                                      :: ncustom_xml
        integer                                      :: NdefaultsMenu
        character(len=24)                            :: defaultsMenu(3)

      end type ciu_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(ciu_struct),pointer,public,save :: object       ! needed for traps.


      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine ciu_create (obj)
      implicit none
      type(ciu_struct),pointer          :: obj                         !argument

      integer                           :: i                           !local
      integer                           :: j                           !local
      integer                           :: window_id                   !local
      type(ciu_window_struct),pointer   :: windowCurrent               !local
      character(len=PC_LENGTH)          :: ctemp                       !local
      character(len=PC_LENGTH)          :: xml_name                    !local
      character(len=PC_LENGTH)          :: title                       !local
      character(len=10)                 :: node                        !local

      nullify(windowCurrent)
      allocate (obj)

      call ciu_window_clear
      call ciu_window_create          (windowCurrent)
      call ciu_window_set_window_type (windowCurrent,'MAIN')
      call ciu_window_set_current     (windowCurrent)
      call ciu_window_get_window_id   (windowCurrent,window_id)
      call ciu_gui_set_main_window    (window_id)

      nullify(obj%ProgList)
      nullify(obj%cwd_dirs)
      nullify(obj%custom_xml_temp)
      nullify(obj%custom_xml)

      call ciu_initialize  (obj)

      call ciu_get_session (obj)

      call getsys_username    (obj%userid)
      call getsys_hostname    (node)
      call getsys_current_dir (obj%working_dir)

      if (obj%lnklib .eq. CIU_PRODLIB) then
        xml_name        = 'guiCIU.xml'
        title           = 'CPS Interactive Utilities'
      else if (obj%lnklib .eq. CIU_BETALIB) then
        xml_name        = 'guiCIU.xml'
        title           = 'CPS Interactive Utilities (test)'
      else
        xml_name        = 'guiCIUCustom.xml'
        title           = 'CPS Interactive Utilities (custom)'
      endif

      call pc_get ('INITIALIZEAPP',ctemp)
      call string_to_upper (ctemp)
      if (trim(ctemp) .eq. 'DIRECTAPPLICATION') then
        obj%exemode = CIU_DIRECTAPP
      else
        obj%exemode = CIU_CSAPP
      endif
      call pc_remove_gui_action ('INITIALIZEAPP','MODIFYFIELD')

      call pc_put_gui_only ('WORKINGDIR','Working Environment = '//  &
                            trim(obj%userid)//'@'//trim(node)//':'//   &
                            trim(obj%working_dir))

      call pc_put_minsize_array ('CIUApps',obj%NProgList)
      call pc_put_maxsize_array ('CIUApps',obj%NProgList)
      call pc_put_gui_only      ('CIUApps',obj%ProgList,obj%NProgList)

      call ciu_gui_create (window_id,trim(title),trim(xml_name),.false.)

      return
      end subroutine ciu_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine ciu_delete (obj)
      implicit none
      type(ciu_struct),pointer          :: obj                         !argument

      type(ciu_window_struct),pointer   :: windowCurrent               !local

      call ciu_save_session (obj)

      if (associated(obj%ProgList))        deallocate(obj%ProgList)
      if (associated(obj%cwd_dirs))        deallocate(obj%cwd_dirs)
      if (associated(obj%custom_xml_temp)) deallocate(obj%custom_xml_temp)
      if (associated(obj%custom_xml))      deallocate(obj%custom_xml)

      call ciu_gui_delete         (obj%mainWindowID,'TERMINATEAPP')
      call ciu_window_get_current (windowCurrent)
      call ciu_window_delete      (windowCurrent)

      deallocate(obj)

      return
      end subroutine ciu_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine ciu_initialize (obj)
      implicit none
      type(ciu_struct),pointer :: obj       ! arguments

      object => obj                                         ! needed for traps.

      allocate(obj%ProgList(1))
      allocate(obj%cwd_dirs(1))
      allocate(obj%custom_xml_temp(1))
      allocate(obj%custom_xml(1))

      obj%stdout             = 6
      obj%lnklib             = ciu_library()
      obj%exemode            = CIU_UNKNOWN
      obj%mainWindowID       = 1
      obj%ScreenId           = 1
      obj%userid             = ' '
      obj%defaults           = 'System Defaults'
      obj%NProgList          = 0
      obj%working_dir        = ' '
      obj%working_dir_temp   = ' '
      obj%ncwd_dirs          = 0
      obj%ncustom_xml_temp   = 0
      obj%ncustom_xml        = 0

      obj%NdefaultsMenu      = 3
      obj%defaultsMenu(1)    = 'System Defaults'
      obj%defaultsMenu(2)    = 'Project Defaults'
      obj%defaultsMenu(3)    = 'User Defaults'

      call prog_list (obj%ProgList,obj%NProgList)

      return
      end subroutine ciu_initialize


!!------------------------------ update ------------------------------------!!
!!------------------------------ update ------------------------------------!!
!!------------------------------ update ------------------------------------!!


      subroutine ciu_update (obj)
      implicit none
      type(ciu_struct),pointer             :: obj                      !argument

      type(ciu_window_struct),pointer      :: windowCurrent            !local
      integer                              :: window_id                !local
      character(len=PC_LENGTH)             :: window_type              !local
      type(ciu_window_struct),pointer      :: windowParent             !local
      character(len=PC_LENGTH)             :: parent_type              !local
      character(len=PC_LENGTH)             :: parent_keyword           !local
      type(prog_struct),pointer          :: prog                   !local
      character(len=PC_LENGTH)             :: ctemp                    !local
      character(len=PC_LENGTH),pointer     :: atemp(:)                 !local
      integer                              :: natemp                   !local
      integer                 ,pointer     :: itemp(:)                 !local
      integer                              :: nitemp                   !local
      integer                              :: indx                     !local
      logical                              :: err                      !local

      object => obj                                           ! needed for traps

      call pc_put_global('CIU_WORKING_DIR',trim(obj%working_dir))

      call ciu_window_get_current (windowCurrent)
      if (.not. associated(windowCurrent)) then
        call pc_error ('Invalid Window')
        return
      endif

      call ciu_window_get_window_id   (windowCurrent,window_id)
      call ciu_window_get_window_type (windowCurrent,window_type)

      select case (trim(window_type))
        case ('MAIN')
          if      (pc_gui_action_present('Exit'            ,'ButtonPress') .or.&
                   pc_gui_action_present('ExitTB'          ,'ButtonPress')) then

            call ciu_delete (obj)

          else if (pc_gui_action_present('ChangeDirectory' ,'ButtonPress') .or.&
                 pc_gui_action_present('ChangeDirectoryTB' ,'ButtonPress')) then

            call ciu_menu_option (obj,'CHANGEDIRECTORY')
            call ciu_menu_update (obj,'CHANGEDIRECTORY')

          else if (pc_gui_action_present('ProgPreferences' ,'ButtonPress') .or.&
                 pc_gui_action_present('ProgPreferencesTB' ,'ButtonPress')) then
            call ciu_menu_option (obj,'PROGPREFERENCES')
            call ciu_menu_update (obj,'PROGPREFERENCES')

          else if (pc_gui_action_present('ApplicationHelp' ,'ButtonPress') .or.&
                 pc_gui_action_present('ApplicationHelpTB' ,'ButtonPress')) then

            call ciu_gui_showhelp()

          else if (pc_gui_action_present('ApplicationVersion',                 &
                                                            'ButtonPress') .or.&
              pc_gui_action_present('ApplicationVersionTB' ,'ButtonPress')) then

            call ciu_menu_option (obj,'APPLICATIONVERSION')

          else if (pc_gui_action_present('DOC'             ,'ButtonPress') .or.&
                   pc_gui_action_present('DOCTB'           ,'ButtonPress')) then

            call ciu_menu_execute (obj,'DOC',err)

          else
            if (pc_gui_action_present('CIUApps','ItemSelected')) then
              call pc_alloc_gui ('CIUApps','ItemSelected',itemp,nitemp)
              if (nitemp .eq. 0) return
              if (itemp(1) .eq. INIL) return
              indx = itemp(1)
              call ciu_prog_create ('CIUApps',indx)
              call ciu_gui_clear_selection  ('CIUApps')
              obj%defaults = 'System Defaults'
              call pc_put_options_field ('programDefaults' ,obj%defaultsMenu , &
                                                            obj%NdefaultsMenu)
              call pc_put ('programDefaults'               ,obj%defaults)
            endif
          endif

        case ('CIU_MENU')
          call ciu_window_get_keyword (windowCurrent ,parent_keyword)

          if      (pc_gui_action_present('OK'              ,'ButtonPress')) then

            call ciu_menu_execute (obj,parent_keyword,err)
            if (.not. err) then
              call ciu_gui_delete        (window_id,'Close')
              call ciu_window_delete     (windowCurrent)
              call ciu_window_get_current(windowCurrent)
            endif

          else if (pc_gui_action_present('Apply'           ,'ButtonPress')) then

            call ciu_menu_execute      (obj,parent_keyword,err)

          else if (pc_gui_action_present('Cancel'          ,'ButtonPress')) then

            call ciu_gui_delete        (window_id,'Close')
            call ciu_window_delete     (windowCurrent)
            call ciu_window_get_current(windowCurrent)

          else if (pc_gui_action_present('Help'            ,'ButtonPress')) then

            call ciu_gui_showhelp()

          else

            call ciu_menu_update       (obj,parent_keyword)
          endif

        case ('PROGRAM')
          call ciu_window_get_keyword (windowCurrent ,parent_keyword)
          call ciu_window_get_prog  (windowCurrent ,prog)

          if      (pc_gui_action_present('OK'              ,'ButtonPress')) then
            call ciu_window_get_parent (windowCurrent,windowParent)
            call ciu_window_get_window_type (windowParent,parent_type)
            if (trim(parent_type) .ne. 'PROGRAM') then
              call prog_execute          (prog)
              call prog_delete           (prog)
              call ciu_gui_delete        (window_id,'Close')
              call ciu_window_delete     (windowCurrent)
              call ciu_window_get_current(windowCurrent)

            else                 ! Child Window of Process

              call prog_update           (prog)
              call ciu_gui_delete        (window_id,'Close')
              call ciu_window_delete     (windowCurrent)
              call ciu_window_get_current(windowCurrent)
            endif

          else if (pc_gui_action_present('Apply'           ,'ButtonPress')) then
            call ciu_window_get_parent (windowCurrent,windowParent)
            call ciu_window_get_window_type (windowParent,parent_type)
            if (trim(parent_type) .ne. 'PROGRAM') then
              call prog_execute (prog)
            else                 ! Child Window of Process
              call prog_update (prog)
            endif

          else if (pc_gui_action_present('Cancel'          ,'ButtonPress')) then
            call ciu_window_get_parent (windowCurrent,windowParent)
            call ciu_window_get_window_type (windowParent,parent_type)
            if (trim(parent_type) .ne. 'PROGRAM') then
              call prog_delete (prog)
            endif
            call ciu_gui_delete        (window_id,'Close')
            call ciu_window_delete     (windowCurrent)
            call ciu_window_get_current(windowCurrent)

          else if (pc_gui_action_present('ProjectDefault'  ,'ButtonPress')) then
            call ciu_save_defaults (prog,'PROJECT DEFAULTS',obj%working_dir)

          else if (pc_gui_action_present('UserDefault'     ,'ButtonPress')) then
            call ciu_save_defaults (prog,'USER DEFAULTS',obj%working_dir)

          else if (pc_gui_action_present('Help'            ,'ButtonPress')) then
            call ciu_gui_showhelp()

          else if (pc_gui_action_present('programDefaults' ,'ModifyField')) then
            call pc_get ('programDefaults' ,obj%defaults)
            call ciu_change_defaults (prog,obj%defaults)

          else if (pc_gui_action_present('Close'           ,'ButtonPress')) then
            call ciu_window_get_parent (windowCurrent,windowParent)
            call ciu_window_get_window_type (windowParent,parent_type)
            if (trim(parent_type) .ne. 'PROGRAM') then
              call prog_delete (prog)
            endif
            call ciu_gui_delete        (window_id,'CLOSE')
            call ciu_window_delete     (windowCurrent)
            call ciu_window_get_current(windowCurrent)

          else
            call prog_update           (prog)
            call pc_put_options_field ('programDefaults' ,obj%defaultsMenu , &
                                                          obj%NdefaultsMenu)
            call pc_put ('programDefaults'               ,obj%defaults)
          endif


        case ('FILEBOX')
          call ciu_window_get_keyword (windowCurrent ,parent_keyword)
          call ciu_window_get_prog  (windowCurrent ,prog)
          if      (pc_gui_action_present('OK'              ,'ButtonPress')) then
            call ciu_gui_delete        (window_id,'CLOSE')
            call ciu_window_delete     (windowCurrent)
            call ciu_window_get_current(windowCurrent)
            call pc_put_gui    (trim(parent_keyword),'WindowType','FILEBOX')
            call prog_update (prog)

          else if (pc_gui_action_present('Cancel'          ,'ButtonPress')) then
            call ciu_gui_delete        (window_id,'CLOSE')
            call ciu_window_delete     (windowCurrent)
            call ciu_window_get_current(windowCurrent)

          else
            call pc_put_gui    (trim(parent_keyword),'WindowType','FILEBOX')
            call prog_update (prog)
          endif

      end select


      return
      end subroutine ciu_update


!!-------------------------------- option ----------------------------------!!
!!-------------------------------- option ----------------------------------!!
!!-------------------------------- option ----------------------------------!!


      subroutine ciu_menu_option (obj,keyword)
      implicit none
      type(ciu_struct),pointer             :: obj                      !argument
      character(len=*),intent(in)          :: keyword                  !argument

      integer                              :: i                        !local
      integer                              :: j                        !local
      character(len=PC_LENGTH)             :: local_key                !local
      type(ciu_window_struct),pointer      :: window                   !local
      integer                              :: window_id                !local
      character(len=PC_LENGTH)             :: xml_name                 !local

      local_key = keyword

      select case (trim(local_key))

        case ('CHANGEDIRECTORY')

          obj%working_dir_temp = obj%working_dir
          call ciu_window_create           (window)
          call ciu_window_set_keyword      (window ,local_key)
          call ciu_window_set_window_type  (window ,'CIU_MENU')
          call ciu_window_get_window_id    (window ,window_id)
          call ciu_gui_create (window_id,'Change Working Directory',  &
                               'ciu_changedirectory.xml',.false.)
          call ciu_window_set_current (window)

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
          call ciu_window_create           (window)
          call ciu_window_set_keyword      (window ,local_key)
          call ciu_window_set_window_type  (window ,'CIU_MENU')
          call ciu_window_get_window_id    (window ,window_id)
          xml_name = 'ciu_progpreferences.xml'
          if (obj%ncustom_xml .gt. 0) then
            do i=1,obj%ncustom_xml
              j = index(obj%custom_xml(i),trim(xml_name),.true.)
              if (j .gt. 0) then
                xml_name = obj%custom_xml(i)
                exit
              endif
            enddo
          endif
          call ciu_gui_create (window_id,'Programmer Preferences',  &
                               trim(xml_name),.false.)
          call ciu_window_set_current (window)

        case ('APPLICATIONVERSION')

          call ciu_gui_clientversion()

        case ('DOC')

      end select

      return
      end subroutine ciu_menu_option


!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!


      subroutine ciu_menu_update (obj,keyword)
      implicit none
      type(ciu_struct),pointer             :: obj                      !argument
      character(len=*),intent(in)          :: keyword                  !argument

      character(len=PC_LENGTH)             :: local_key                !local
      character(len=PC_LENGTH)             :: filename                 !local
      character(len=PC_LENGTH)             :: filter                   !local
      type(ciu_window_struct),pointer      :: window                   !local
      integer                              :: window_id                !local
      integer                              :: j1                       !local
      integer                              :: j2                       !local
      integer                              :: indx                     !local
      integer,pointer                      :: itmp_array(:)            !local

      local_key = keyword

      select case (trim(local_key))

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
              j2 = index(trim(obj%working_dir_temp),'/',.true.)
              if (j1.eq.j2 .and. j2.gt.0) then
                j2 = index(obj%working_dir_temp(1:j1-1),'/',.true.)
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
          j2 = index(trim(obj%working_dir_temp),'/',.true.)
          if (j1 .ne. j2) obj%working_dir_temp(j1+1:j1+1) = '/'
          call ciu_menu_load_cwd_dirs (obj)

          call pc_put_minsize_array ('DIRECTORIES',obj%ncwd_dirs)
          call pc_put_maxsize_array ('DIRECTORIES',obj%ncwd_dirs)

          call pc_put ('WORKING_DIR', obj%working_dir_temp)
          call pc_put ('DIRECTORIES', obj%cwd_dirs        , obj%ncwd_dirs)

        case ('PROGPREFERENCES')
          call pc_alloc ('CUSTOM_XML',obj%custom_xml_temp,obj%ncustom_xml_temp)
          call pc_put   ('CUSTOM_XML',obj%custom_xml_temp,obj%ncustom_xml_temp)

        case ('APPLICATIONVERSION')

        case ('DOC')

      end select

      return
      end subroutine ciu_menu_update


!!------------------------------- execute ----------------------------------!!
!!------------------------------- execute ----------------------------------!!
!!------------------------------- execute ----------------------------------!!


      subroutine ciu_menu_execute (obj,keyword,err)
      implicit none
      type(ciu_struct),pointer             :: obj                      !argument
      character(len=*),intent(in)          :: keyword                  !argument
      logical         ,intent(out)         :: err                      !argument

      character(len=PC_LENGTH)             :: local_key                !local
      character(len=PC_LENGTH)             :: filename                 !local
      character(len=PC_LENGTH)             :: filter                   !local
      character(len=PC_LENGTH)             :: msg                      !local
      character(len=FILENAME_LENGTH)       :: cpsdoc_file              !local
      character(len=PC_LENGTH),pointer     :: cards(:)                 !local
      character(len=PC_LENGTH)             :: node                     !local
      character(len=PC_LENGTH)             :: cpsdoc_node              !local
      type(ciu_window_struct),pointer      :: window                   !local
      integer                              :: window_id                !local
      integer                              :: i                        !local
      integer                              :: j                        !local
      integer                              :: ncards                   !local
      integer                              :: istat                    !local

      err = .false.
      local_key = keyword

      select case (trim(local_key))

        case ('CHANGEDIRECTORY')

          call ciu_inquire_directory (obj%working_dir_temp,istat,msg)
          if (istat .ne. CIU_NAME_VALID) then
            err = .true.
            call pc_error (msg)
          else
            istat = finquire_output(trim(obj%working_dir_temp)//'.xyz')
            if (istat .eq. FINQUIRE_ERROR) then
              err = .true.
              call pc_error ('Can not create files in this directory')
            else
              obj%working_dir = obj%working_dir_temp
              call getsys_hostname (node)
              call pc_put_gui_only ('WORKINGDIR','Working Environment = '//  &
                                    trim(obj%userid)//'@'//trim(node)//':'//  &
                                    trim(obj%working_dir))
            endif
          endif

        case ('PROGPREFERENCES')

          if (associated(obj%custom_xml)) deallocate(obj%custom_xml)
          obj%ncustom_xml = obj%ncustom_xml_temp
          if (obj%ncustom_xml .gt. 0) then
            allocate(obj%custom_xml(obj%ncustom_xml))
            obj%custom_xml = obj%custom_xml_temp
          else
            allocate(obj%custom_xml(1))
            obj%custom_xml = ' '
          endif

        case ('APPLICATIONVERSION')

        case ('DOC')

          if (obj%exemode .eq. CIU_DIRECTAPP) then
            cpsdoc_file = ''
            cpsdoc_node = ''
            call cnfg_get_value ('CPSDOC_NODES_FILE',cpsdoc_file)
            if (len_trim(cpsdoc_file) .gt. 0) then
              call ciu_read_file (cpsdoc_file,cards,ncards)
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
              write(STDOUT,*) 'netscape http://'//trim(cpsdoc_node)//  &
                              '/cgi-sps/cps_doc.pl'
              call putsys_texec ('netscape http://'//trim(cpsdoc_node)//  &
                                 '/cgi-sps/cps_doc.pl &',istat)
            else
              call pc_error ('Netscape not found.  Can not display DOC')
            endif
          else
            call pc_error ('This option is not supported in client/server mode')
          endif

      end select

      return
      end subroutine ciu_menu_execute


!!--------------------------- load_cwd_dirs -------------------------------!!
!!--------------------------- load_cwd_dirs -------------------------------!!
!!--------------------------- load_cwd_dirs -------------------------------!!


      subroutine ciu_menu_load_cwd_dirs(obj)
      implicit none
      type(ciu_struct),pointer             :: obj                      !argument

      character(len=PC_LENGTH)             :: ctemp                    !local
      integer,allocatable                  :: htemp(:)                 !local
      integer                              :: nhtemp                   !local
      integer                              :: i                        !local
      integer                              :: i1                       !local
      integer                              :: i2                       !local
      integer                              :: j                        !local
      integer                              :: j1                       !local
      integer                              :: j2                       !local
      integer                              :: cpi                      !local
      integer                              :: itype                    !local
      integer,external                     :: filebox_opendir          !local
      integer,external                     :: filebox_filter           !local
      integer,external                     :: filebox_readdir          !local
      integer,external                     :: filebox_closedir         !local
      integer,external                     :: filebox_sort             !local

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
            call ciu_append_array_element (obj%cwd_dirs,obj%ncwd_dirs,  &
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
      end subroutine ciu_menu_load_cwd_dirs


!!------------------------------- create -----------------------------------!!
!!------------------------------- create -----------------------------------!!
!!------------------------------- create -----------------------------------!!


      subroutine ciu_prog_create (keyword,indx)
      implicit none
      character(len=*),intent(in)       :: keyword                     !argument
      integer         ,intent(in)       :: indx                        !argument

      character(len=PC_LENGTH)          :: local_key                   !local
      type(ciu_window_struct),pointer   :: window                      !local
      integer                           :: window_id                   !local
      character(len=PC_LENGTH)          :: window_type                 !local
      character(len=PC_LENGTH)          :: window_title                !local
      type(prog_struct),pointer         :: prog                        !local
      integer                           :: prog_id                     !local
      character(len=PC_LENGTH)          :: prog_name                   !local
      character(len=PC_LENGTH)          :: xml_name                    !local
      character(len=PC_LENGTH),pointer  :: proglist(:)                 !local
      integer                           :: nproglist                   !local
      integer                           :: i                           !local
      integer                           :: j                           !local
      type(cardset_struct),pointer      :: cardset                     !local
      integer                           :: nkeys                       !local
      character(len=PC_LENGTH),pointer  :: keys(:)                     !local
      integer                           :: nextrakeys = 7              !local
      character(len=PC_LENGTH)          :: extrakeys(7)                !local
      integer                           :: pc_lun                      !local

      
      nullify(keys)
      nkeys = 0

      local_key = keyword
      window_type  = 'PROGRAM'
      prog_id   = indx
      prog_name = object%ProgList(prog_id)
      window_title = trim(prog_name)

      window_id=ciu_window_match (window_type,local_key,prog_name,prog_id)
      if (window_id .ne. -1) then
        call ciu_gui_jump_window (window_id)
        return
      endif

      call ciu_window_create          (window)
      call ciu_window_set_window_type (window,window_type)
      call ciu_window_set_keyword     (window ,local_key)
      call ciu_window_set_value       (window ,prog_name)
      call ciu_window_set_index       (window ,prog_id)

      call pc_restore
      pc_lun = pclun_get()
      call pc_frontend_update         (pc_lun)
      call prog_create                (prog, prog_name)
      call ciu_window_set_prog        (window,prog)

      if (len_trim(prog_name) .eq. 0) then
        xml_name = 'none.xml'
      else
        xml_name = trim(prog_name) // '.xml'
      endif

      call string_to_lower (xml_name)
      call ciu_window_get_window_id (window,window_id)

      if (object%lnklib .eq. CIU_UNKNOWN) then
        if (object%ncustom_xml .gt. 0) then
          do i=1,object%ncustom_xml
            j = index(object%custom_xml(i),'/',.true.)
            if (j .gt. 0) then
              if (trim(object%custom_xml(i)(j+1:)) .eq. trim(xml_name)) then
                xml_name = object%custom_xml(i)
                exit
              endif
            endif
          enddo
        endif
      endif

      if (pc_gui_action_present('PROG','WINDOWKEYS')) then
        extrakeys( 1) = 'OK'
        extrakeys( 2) = 'APPLY'
        extrakeys( 3) = 'CANCEL'
        extrakeys( 4) = 'HELP'
        extrakeys( 5) = 'ERROR'
        extrakeys( 6) = 'WARNING'
        extrakeys( 7) = 'INFO'
        nkeys = pc_num_elements_gui ('PROG','WINDOWKEYS')
        if (nkeys .gt. 0) then
          allocate(keys(nkeys+nextrakeys))
          call pc_get_gui ('PROG','WINDOWKEYS',keys,nkeys)
          do i=1,nkeys
            call string_to_upper (keys(i))
          enddo
          keys(nkeys+1:nkeys+nextrakeys) = extrakeys
          nkeys = nkeys + nextrakeys
          call ciu_window_set_keys (window,keys,nkeys)
          deallocate(keys)
        endif
      endif

      call ciu_gui_create (window_id,trim(window_title),trim(xml_name),.false.)

      call ciu_window_set_current (window)

      return
      end subroutine ciu_prog_create


!!-------------------------- inquire_directory ----------------------------!!
!!-------------------------- inquire_directory ----------------------------!!
!!-------------------------- inquire_directory ----------------------------!!


      subroutine ciu_inquire_directory (directory,status,msg)
      implicit none
      character(len=*),intent(inout) :: directory                 ! arguments
      integer         ,intent(out)   :: status                    ! arguments
      character(len=*),intent(out)   :: msg                       ! arguments

      integer,external               :: filebox_opendir           ! local
      character(len=PC_LENGTH)       :: ctemp                     ! local
      integer,allocatable            :: htemp(:)                  ! local
      integer                        :: nhtemp                    ! local
      integer                        :: cpi                       ! local
      integer                        :: i                         ! local
      integer                        :: j                         ! local

      cpi = string_chars_per_integer()
      nhtemp = (PC_LENGTH+1)/cpi + 1
      allocate(htemp(nhtemp))
      htemp = 0

      j = len_trim(directory)
      if (directory(j:j) .eq. '/') then
        ctemp = directory(1:j-1)
      else
        ctemp = directory
      endif
      call string_cc2hh(trim(ctemp),htemp)
      i = filebox_opendir(htemp)
      if (i .ne. 0) then
        msg = 'Invalid Directory'
        status = CIU_NAME_INVALID
        deallocate(htemp)
        return
      endif

      msg = 'Valid Directory'
      status = CIU_NAME_VALID

      deallocate(htemp)
      return
      end subroutine ciu_inquire_directory


!!---------------------------- save_session -------------------------------!!
!!---------------------------- save_session -------------------------------!!
!!---------------------------- save_session -------------------------------!!


      subroutine ciu_save_session (obj)
      implicit none
      type(ciu_struct),pointer             :: obj                      !argument

      character(len=PC_LENGTH)             :: home_dir                 !local
      character(len=PC_LENGTH)             :: filename                 !local
      character(len=PC_LENGTH),pointer     :: cards(:)                 !local
      integer                              :: ncards                   !local
      integer                              :: lun                      !local
      integer                              :: istat                    !local
      integer                              :: i                        !local
      type(cardset_struct),pointer         :: cardset                  !local
      character(len=2)                     :: mode                     !local
      character(len=PC_DATACARD_LENGTH)    :: string                   !local
      integer                              :: nstring                  !local
      integer                              :: temp


      nullify(cards)
      nullify(cardset)

      call getsys_env ('HOME',home_dir)
      filename = trim(home_dir)//'/.ciu_defaults'

      mode = "w";
      lun = cio_fopen(trim(filename),mode);
      if (lun .lt. 100) then
        write(obj%stdout,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      call cardset_create (cardset)
      call cardset_set_packing_option (cardset,CARDSET_PACKED)

      call cardset_put_array  (cardset,'CUSTOM_XML',obj%custom_xml,  &
                                                    obj%ncustom_xml)

      ncards = 0
      call cardset_alloc_cards (cardset,cards,ncards)
      if (ncards .gt. 0) then
        do i=1,ncards
          string  = cards(i)
          temp      = len_trim(string)
          nstring   = cio_fputline(string,temp,lun)
          if (nstring .lt. len_trim(string)) then
            write(obj%stdout,*) 'Error writing session file  ',nstring,  &
                                 len_trim(string)
          endif
        enddo
      endif
      istat = cio_fclose(lun)

      call cardset_delete (cardset)
      if (associated(cards)) deallocate(cards)

      return
      end subroutine ciu_save_session


!!----------------------------- get_session -------------------------------!!
!!----------------------------- get_session -------------------------------!!
!!----------------------------- get_session -------------------------------!!


      subroutine ciu_get_session (obj)
      implicit none
      type(ciu_struct),pointer             :: obj                      !argument

      character(len=PC_LENGTH)             :: home_dir                 !local
      character(len=PC_LENGTH)             :: filename                 !local
      character(len=PC_LENGTH)             :: errmsg                   !local
      integer                              :: istat                    !local
      integer                              :: lun = 0                  !local
      type(cardset_struct),pointer         :: cardset                  !local
      character(len=2)                     :: mode                     !local
      character(len=PC_DATACARD_LENGTH)    :: string                   !local
      integer                              :: nstring                  !local


      call getsys_env ('HOME',home_dir)
      filename = trim(home_dir)//'/.ciu_defaults'

      mode = "r";
      lun = cio_fopen(trim(filename),mode);
      if (lun .lt. 100) then
        write(obj%stdout,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      nullify(cardset)
      call cardset_create (cardset)
      call cardset_set_packing_option (cardset,CARDSET_PACKED)

      do
        nstring = cio_fgetline(string,PC_DATACARD_LENGTH,lun)
        if (nstring .lt. 0) exit
        call cardset_add_card (cardset,string(1:nstring))
      enddo

      istat = cio_fclose(lun)

      call cardset_alloc_array(cardset,'CUSTOM_XML',obj%custom_xml ,  &
                                                    obj%ncustom_xml,errmsg)
      call cardset_delete (cardset)

      return
      end subroutine ciu_get_session

!!---------------------------- save_defaults -------------------------------!!
!!---------------------------- save_defaults -------------------------------!!
!!---------------------------- save_defaults -------------------------------!!


      subroutine ciu_save_defaults (prog, type, working_dir)
      implicit none
      type(prog_struct),pointer       :: prog                      !argument
      character(len=*),intent(in)       :: type                        !argument
      character(len=*),intent(in)       :: working_dir                 !argument

      character(len=PC_LENGTH)          :: local_type                  !local
      character(len=PC_LENGTH)          :: defaults_dir                !local
      character(len=PC_LENGTH)          :: prog_name                 !local
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
      integer                           :: nstring = 132               !local
      integer                           :: temp

      call getsys_env ('HOME',home_dir)
      local_type = type
      call string_to_upper (local_type)
      select case (trim(local_type))
        case ('USER DEFAULTS')
          defaults_dir = trim(home_dir)//'/.ciu_user_defaults'
        case ('PROJECT DEFAULTS')
          defaults_dir = trim(working_dir)//'/.ciu_project_defaults'
      end select

      call prog_get_name (prog,prog_name)
      call string_to_lower (prog_name)
      filename = trim(defaults_dir)//'/'//trim(prog_name)//'.def'

      inquire (file=trim(defaults_dir),exist=exists)
      if (.not. exists) call putsys_texec ('mkdir '// trim(defaults_dir),istat)

      mode = "w"
      lun = cio_fopen(trim(filename),mode)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      string = '<MASTER name="' // trim(prog_name) // '">'
      temp  = len_trim(string)
      istat = cio_fputline(string,temp,lun)
      nullify(cards)
      call prog_alloc_gui_cards (prog, cards, ncards)
      if (ncards .gt. 0) then
        string = ' <GUI>'
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun)
        do i=1,ncards
          string = '  ' // cards(i)
          temp  = len_trim(string)
          istat = cio_fputline(string,temp,lun)
        enddo
        string = ' </GUI>'
        temp  = len_trim(string)
        istat = cio_fputline(string,temp,lun)
      endif
      string = '</MASTER>'
      temp  = len_trim(string)
      istat = cio_fputline(string,temp,lun)
      istat = cio_fclose(lun)
      if (associated(cards)) deallocate(cards)

      return
      end subroutine ciu_save_defaults


!!---------------------------- get_defaults --------------------------------!!
!!---------------------------- get_defaults --------------------------------!!
!!---------------------------- get_defaults --------------------------------!!


      subroutine ciu_get_defaults (prog, type, working_dir, cards, ncards)
      implicit none
      type(prog_struct),pointer       :: prog                      !argument
      character(len=*),intent(in)       :: type                        !argument
      character(len=*),intent(in)       :: working_dir                 !argument
      character(len=PC_LENGTH),pointer  :: cards(:)                    !argument
      integer                           :: ncards                      !argument

      character(len=PC_LENGTH)          :: local_type                  !local
      character(len=PC_LENGTH)          :: defaults_dir                !local
      character(len=PC_LENGTH)          :: prog_name                 !local
      character(len=PC_LENGTH)          :: home_dir                    !local
      character(len=PC_LENGTH)          :: filename                    !local
      character(len=PC_LENGTH)          :: tag_card_sav                !local
      character(len=PC_LENGTH),pointer  :: temp_cards(:)               !local
      integer                           :: lun                         !local
      integer                           :: istat                       !local
      character(len=2)                  :: mode                        !local
      character(len=133)                :: string                      !local
      integer                           :: nstring = 132               !local

      ncards = 0
      call getsys_env ('HOME',home_dir)
      local_type = type
      call string_to_upper (local_type)
      select case (trim(local_type))
        case ('USER DEFAULTS')
          defaults_dir = trim(home_dir)//'/.ciu_user_defaults'
        case ('PROJECT DEFAULTS')
          defaults_dir = trim(working_dir)//'/.ciu_project_defaults'
      end select

      call prog_get_name (prog,prog_name)
      call string_to_lower (prog_name)
      filename = trim(defaults_dir)//'/'//trim(prog_name)//'.def'

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
        if (string(1:1) .eq. '<') then                       ! MASTER card
          if (string(1:2) .eq. '</') exit                    ! end MASTER card

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
      end subroutine ciu_get_defaults


!!--------------------------- change_defaults ------------------------------!!
!!--------------------------- change_defaults ------------------------------!!
!!--------------------------- change_defaults ------------------------------!!


      subroutine ciu_change_defaults (prog,defaults)
      implicit none
      type(prog_struct),pointer         :: prog                        !argument
      character(len=*)                  :: defaults                    !argument

      character(len=PC_LENGTH)          :: prog_defaults               !local
      character(len=PC_LENGTH),pointer  :: cards(:)                    !local
      integer                           :: ncards                      !local

      prog_defaults = trim(defaults)
      call string_to_upper (prog_defaults)

      select case (trim(prog_defaults))
        case ('SYSTEM DEFAULTS')
          call prog_initialize (prog)
        case ('PROJECT DEFAULTS','USER DEFAULTS')
          nullify(cards)
          call ciu_get_defaults (prog, prog_defaults,   &
                                 object%working_dir, cards, ncards)
          if (ncards .gt. 0) then
            call pc_put_gui_cards (cards,ncards)
            if (associated(cards)) deallocate(cards)
            call prog_update (prog)
          else
            call pc_error('No '// trim(prog_defaults) // ' found')
          endif
      end select

      return
      end subroutine ciu_change_defaults


!!------------------------ append_array_element ----------------------------!!
!!------------------------ append_array_element ----------------------------!!
!!------------------------ append_array_element ----------------------------!!


      subroutine ciu_append_array_element (array,narray,value,repeat)
      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(inout)           :: narray               !argument
      character(len=*),intent(in)              :: value                !argument
      integer,optional                         :: repeat               !argument

      integer                                  :: i                    !local
      integer                                  :: nadd                 !local
      character(len=PC_LENGTH),allocatable     :: temp_array(:)        !local

      if (present(repeat)) then
        if (repeat .gt. 0) then
          nadd = repeat
        else
          nadd = 1
        endif
      else
        nadd = 1
      endif

      if (.not. associated(array)) then
        narray = nadd
        allocate(array(narray))
        do i=1,narray
          array(i) = value
        enddo
        return
      else
        allocate(temp_array(narray))
        temp_array(1:narray) = array(1:narray)
        deallocate(array)
        allocate(array(narray+nadd))
        array(1:narray) = temp_array(1:narray)
        do i=1,nadd
          array(narray+i:narray+i) = value
        enddo
        narray = narray + nadd
        deallocate(temp_array)
      endif

      return
      end subroutine ciu_append_array_element


!!------------------------------- library ---------------------------------!!
!!------------------------------- library ---------------------------------!!
!!------------------------------- library ---------------------------------!!


      function ciu_library () result (lnklib)
      implicit none
      integer                  :: lnklib                        ! result

      integer,external         :: ciu_library_c                 ! local

      lnklib = ciu_library_c()

      return
      end function ciu_library

!!------------------------------ read_file --------------------------------!!
!!------------------------------ read_file --------------------------------!!
!!------------------------------ read_file --------------------------------!!


      subroutine ciu_read_file (filename,cards,ncards)
      implicit none
      character(len=*),intent(in)     :: filename                      !argument
      character(len=*),pointer        :: cards(:)                      !argument
      integer         ,intent(out)    :: ncards                        !argument

      integer                         :: lun                           !local
      integer                         :: nstr                          !local
      integer                         :: istat                         !local
      character(len=1024)             :: str                           !local

      if (associated(cards)) deallocate(cards)
      ncards = 0

      lun = cio_fopen(trim(filename),'r')
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(filename)//'  lun=',lun
        return
      endif

      do
        nstr = cio_fgetline(str,1024,lun)
        if (nstr .lt. 0) exit
        call ciu_append_array_element (cards,ncards,trim(str))
      enddo

      istat = cio_fclose(lun)

      return
      end subroutine ciu_read_file



!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module ciu_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

