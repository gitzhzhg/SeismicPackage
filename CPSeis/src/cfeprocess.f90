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
! Name       : cfeprocess 
! Category   : cfe
! Written    : 2000-08-15   by: Donna K. Vunderink
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : CFE Process Module.
! Portability: No known limitations, but see note below.
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
!013. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
! 12. 2003-11-18  Stoeckley    Provide workarounds for Portland Group compiler.
! 11. 2003-09-15  Stoeckley    Changed name from cfe_process to cfeprocess;
!                               changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
! 10. 2002-09-23  Vunderink    Changed prints to writes for checkc.
!  9. 2002-09-10  Vunderink    Added delete_all_popups.
!  8. 2002-06-07  Vunderink    If same_window is true and a different window is
!                                already open to this process, close it.
!  7. 2001-08-09  Vunderink    Check custom_xml only when running cfecustom.
!  6. 2001-08-08  Vunderink    Create empty process screen for blank process
!                                names in workfile and ignore error when trying
!                                to update them.
!  5. 2000-10-16  Vunderink    Added control for MWBLABEL.
!  4. 2000-10-13  Vunderink    Fixed typo causing project default button to
!                                be insensitive.
!  3. 2000-09-18  Vunderink    Start running traps after the current process.
!  2. 2000-09-04  Vunderink    Eliminate un-necessary calls to string_to_upper
!                                improve logic on when to run_all_traps.
!  1. 2000-08-15  Vunderink    Initial version.
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


      module cfeprocess_module

      use cfestruct_module
      use cfegui_module
      use cfewindow_module
      use process_module
      use workfile_module
      use cardset_module
      use pc_module
      use string_module

      implicit none

      private
      public :: cfeprocess_create
      public :: cfeprocess_delete
      public :: cfeprocess_update
      public :: cfeprocess_reset
      public :: cfeprocess_delete_all_popups
      public :: cfeprocess_delete_popup
      public :: cfeprocess_update_all_popups

      character(len=100),public,save :: cfeprocess_ident = &
       '$Id: cfeprocess.f90,v 1.13 2006/09/18 13:32:42 Glover prod sps $'

      contains


!!------------------------------- create -----------------------------------!!
!!------------------------------- create -----------------------------------!!
!!------------------------------- create -----------------------------------!!


      subroutine cfeprocess_create (keyword,indx,same_window)
      implicit none
      character(len=*),intent(in)       :: keyword                     !argument
      integer         ,intent(in)       :: indx                        !argument
      logical,optional                  :: same_window                 !argument

      character(len=PC_LENGTH)          :: local_key                   !local
      type(cfewindow_struct),pointer    :: window                      !local
      integer                           :: window_id                   !local
      character(len=PC_LENGTH)          :: window_type                 !local
      character(len=PC_LENGTH)          :: window_title                !local
      type(process_struct),pointer      :: process                     !local
      type(process_struct),pointer      :: process_in_list             !local
      type(process_struct),pointer      :: previous,next               !local
      integer                           :: process_id                  !local
      character(len=PC_LENGTH)          :: process_name                !local
      character(len=PC_LENGTH)          :: xml_name                    !local
      character(len=10)                 :: cid                         !local
      character(len=10)                 :: ctotal                      !local
      character(len=PC_LENGTH),pointer  :: processlist(:)              !local
      integer                           :: nprocesslist                !local
      character(len=PC_LENGTH),pointer  :: cards(:)                    !local
      integer                           :: ncards                      !local
      integer                           :: i                           !local
      integer                           :: j                           !local
      logical                           :: readonly_flag               !local
      type(cardset_struct),pointer      :: cardset                     !local
      integer                           :: nkeys                       !local
      character(len=PC_LENGTH),pointer  :: keys(:)                     !local
      integer                           :: nextrakeys = 15             !local
      character(len=PC_LENGTH)          :: extrakeys(15)               !local

      nullify (previous) ! jpa
      nullify (next) ! jpa
      nullify (cardset) ! jpa
      nullify (process) ! jpa
      nullify (window) ! jpa
      nullify (keys)
      nkeys = 0

      local_key = keyword
!     call string_to_upper (local_key)
      select case (trim(local_key))
        case ('CURRENTPROCESSLIST')
          window_type  = 'PROCESS'
          process_id   = indx
          process_name = object%currentProcessList(process_id)
          call string_ii2cc (process_id,cid)
          call string_ii2cc (object%NcurrentProcessList,ctotal)
          window_title = trim(object%currentJobName) // '   ----------   ' //  &
                         trim(process_name)          // '   ----------   ' //  &
                         'Process ' // trim(cid) // ' of ' // trim(ctotal)
        case ('OLDPROCESSLIST')
          window_type  = 'OLDPROCESS'
          process_id   = indx
          process_name = object%oldProcessList(process_id)
          call string_ii2cc (process_id,cid)
          call string_ii2cc (object%NoldProcessList,ctotal)
          window_title = trim(object%oldJobName) // '   ----------   ' //    &
                         trim(process_name)      // '   ----------   ' //    &
                         'Process ' // trim(cid) // ' of ' // trim(ctotal)
        case ('SUBSETPROCESSLIST')
          window_type  = 'LISTPROCESS'
          process_name = object%subsetProcessList(indx)
          if (trim(process_name) .eq. 'PROJECT_DATA') then
            process_id = 1
          else if (trim(process_name) .eq. 'JOB_DATA') then
            process_id = 2
          else
            process_id   = indx
          endif
          window_title = trim(process_name)
        case ('TEMPLATEPROCESSLIST')
          window_type  = 'TEMPLATEPROCESS'
          process_id   = indx
          process_name = object%templateProcessList(process_id)
          call string_ii2cc (process_id,cid)
          call string_ii2cc (object%NtemplateProcessList,ctotal)
          window_title = trim(object%templateJobName) // '   ----------   ' // &
                         trim(process_name)      // '   ----------   ' //    &
                         'Process ' // trim(cid) // ' of ' // trim(ctotal)
      end select

      window_id=cfewindow_match (window_type,local_key,process_name,process_id)
      if (window_id .ne. -1) then
        if (present(same_window)) then
          if (same_window) then
            call cfegui_delete         (window_id,'Close')
            call cfewindow_get_pointer (window_id,window)
            call cfewindow_delete      (window)
          else
            call cfegui_jump_window (window_id)
            return
          endif
        else
          call cfegui_jump_window (window_id)
          return
        endif
      endif

      if (present(same_window)) then
        if (same_window) then
          call cfewindow_get_current (window)
          call cfewindow_get_cardset (window,cardset)
          call cardset_clear         (cardset)
          call cfewindow_set_cardset (window,cardset)
          call cfewindow_set_keys    (window,keys,nkeys)
        else
          call cfewindow_create      (window)
        endif
      else
        call cfewindow_create        (window)
      endif
      call cfewindow_set_window_type (window,window_type)
      call cfewindow_set_keyword     (window ,local_key)
      call cfewindow_set_value       (window ,process_name)
      call cfewindow_set_index       (window ,process_id)

      select case (trim(local_key))
        case ('CURRENTPROCESSLIST')
          call workfile_get_first_process (object%currentWorkfile,process)
          if (process_id .gt. 1) then
            do i = 2, process_id
              if (.not. associated(process)) exit
!!!           call process_get_next (process,process)
              call process_get_next (process,next)
              process => next
            enddo
          endif
          process_in_list => process
          call process_create (process, process_name, process_id)
          call cfewindow_set_process    (window,process)
          call process_get_previous      (process_in_list,previous)
          call process_set_previous      (process,previous)
          call process_copy_pdata_cards  (previous,process)
          call process_copy_jdata_cards  (previous,process)
          call process_copy_global_cards (previous,process)
          call process_copy              (process_in_list, process)
          call process_set_mate          (process_in_list, process)
          nullify(cards)
          call process_alloc_gui_cards (process, cards, ncards)
          call pc_put_gui_cards (cards, ncards)
          call pc_remove_gui_action ('ERROR'  , 'ERROR')
          call pc_remove_gui_action ('WARNING', 'WARNING')
          call pc_remove_gui_action ('INFO'   , 'INFO')
          if (associated(cards)) deallocate(cards)
          nprocesslist = object%NcurrentProcessList
          allocate(processlist(nprocesslist))
          do i=1,nprocesslist
            call string_ii2cc (i,cid)
            processlist(i) = trim(cid)//' '//trim(object%currentProcessList(i))
          enddo
          call pc_put_options_field ('PROCESSDEFAULTS' ,           &
                                      object%processDefaultsMenu , &
                                      object%NprocessDefaultsMenu)
          call pc_put               ('PROCESSDEFAULTS',object%processDefaults)
          call pc_put_options_field ('PROCESSLIST',processlist,nprocesslist)
          call pc_put_gui_only ('PROCESSLIST',processlist(process_id))
          if (associated(processlist)) deallocate(processlist)
          call pc_put_gui ('Close' ,'Visible','False')
          call pc_put_gui ('MWBLABEL','Visible','False')
          readonly_flag = .false.
        case ('OLDPROCESSLIST')
          call workfile_get_first_process (object%oldWorkfileDisplayed,process)
          if (process_id .gt. 1) then
            do i = 2, process_id
              if (.not. associated(process)) exit
!!!           call process_get_next (process,process)
              call process_get_next (process,next)
              process => next
            enddo
          endif
          call cfewindow_set_process (window,process)
          nullify(cards)
          call process_alloc_gui_cards (process, cards, ncards)
          call pc_put_gui_cards (cards, ncards)
          call pc_remove_gui_action ('ERROR'  , 'ERROR')
          call pc_remove_gui_action ('WARNING', 'WARNING')
          call pc_remove_gui_action ('INFO'   , 'INFO')
          if (associated(cards)) deallocate(cards)
          nprocesslist = object%NoldProcessList
          allocate(processlist(nprocesslist))
          do i=1,nprocesslist
            call string_ii2cc (i,cid)
            processlist(i) = trim(cid)//' '//trim(object%oldProcessList(i))
          enddo
          call pc_put_options_field ('PROCESSLIST',processlist,nprocesslist)
          call pc_put_gui_only ('PROCESSLIST',processlist(process_id))
          if (associated(processlist)) deallocate(processlist)
          call pc_put_gui ('Close'           ,'SensitiveButton','True' )
          call pc_put_gui ('ProcessListLeft' ,'SensitiveButton','True' )
          call pc_put_gui ('ProcessList'     ,'SensitiveButton','True' )
          call pc_put_gui ('ProcessListRight','SensitiveButton','True' )
          call pc_put_gui ('ProjectDefault'  ,'SensitiveButton','True' )
          call pc_put_gui ('UserDefault'     ,'SensitiveButton','True' )
          call pc_put_gui ('Help'            ,'SensitiveButton','True' )
          call pc_put_gui ('PROCESSDEFAULTS' ,'Visible'        ,'False')
          call pc_put_gui ('MWBLABEL'        ,'Visible'        ,'False')
          call pc_put_gui ('OK'              ,'Visible'        ,'False')
          call pc_put_gui ('Apply'           ,'Visible'        ,'False')
          call pc_put_gui ('Cancel'          ,'Visible'        ,'False')
          call pc_put_gui ('Reset'           ,'Visible'        ,'False')
          readonly_flag = .true.
        case ('SUBSETPROCESSLIST')
          call process_create (process, process_name, process_id)
          call process_create_super (process)
          call cfewindow_set_process (window,process)
          nullify(cards)
          call process_alloc_gui_cards (process, cards, ncards)
          call pc_put_gui_cards (cards, ncards)
          call pc_remove_gui_action ('ERROR'  , 'ERROR')
          call pc_remove_gui_action ('WARNING', 'WARNING')
          call pc_remove_gui_action ('INFO'   , 'INFO')
          if (associated(cards)) deallocate(cards)
          nprocesslist = object%NsubsetProcessList
          allocate(processlist(nprocesslist))
          do i=1,nprocesslist
            call string_ii2cc (i,cid)
            processlist(i) = trim(cid)//' '//trim(object%subsetProcessList(i))
          enddo
          call pc_put_options_field ('PROCESSLIST',processlist,nprocesslist)
          call pc_put_gui_only ('PROCESSLIST',processlist(process_id))
          if (associated(processlist)) deallocate(processlist)
          call pc_put_gui ('Close'           ,'SensitiveButton','True' )
          call pc_put_gui ('Help'            ,'SensitiveButton','True' )
          call pc_put_gui ('PROCESSDEFAULTS' ,'Visible'        ,'False')
          call pc_put_gui ('MWBLABEL'        ,'Visible'        ,'False')
          call pc_put_gui ('OK'              ,'Visible'        ,'False')
          call pc_put_gui ('Apply'           ,'Visible'        ,'False')
          call pc_put_gui ('Cancel'          ,'Visible'        ,'False')
          call pc_put_gui ('Reset'           ,'Visible'        ,'False')
          call pc_put_gui ('ProcessListLeft' ,'Visible'        ,'False')
          call pc_put_gui ('ProcessList'     ,'Visible'        ,'False')
          call pc_put_gui ('ProcessListRight','Visible'        ,'False')
          call pc_put_gui ('ProjectDefault'  ,'Visible'        ,'False')
          call pc_put_gui ('UserDefault'     ,'Visible'        ,'False')
          readonly_flag = .true.
        case ('TEMPLATEPROCESSLIST')
          call workfile_get_first_process (object%templateWorkfile,process)
          if (process_id .gt. 1) then
            do i = 2, process_id
              if (.not. associated(process)) exit
!!!           call process_get_next (process,process)
              call process_get_next (process,next)
              process => next
            enddo
          endif
          call cfewindow_set_process (window,process)
          nullify(cards)
          call process_alloc_gui_cards (process, cards, ncards)
          call pc_put_gui_cards (cards, ncards)
          call pc_remove_gui_action ('ERROR'  , 'ERROR')
          call pc_remove_gui_action ('WARNING', 'WARNING')
          call pc_remove_gui_action ('INFO'   , 'INFO')
          if (associated(cards)) deallocate(cards)
          nprocesslist = object%NtemplateProcessList
          allocate(processlist(nprocesslist))
          do i=1,nprocesslist
            call string_ii2cc (i,cid)
            processlist(i) = trim(cid)//' '//trim(object%templateProcessList(i))
          enddo
          call pc_put_options_field ('PROCESSLIST',processlist,nprocesslist)
          call pc_put_gui_only ('PROCESSLIST',processlist(process_id))
          if (associated(processlist)) deallocate(processlist)
          call pc_put_gui ('Close'           ,'SensitiveButton','True' )
          call pc_put_gui ('ProcessListLeft' ,'SensitiveButton','True' )
          call pc_put_gui ('ProcessList'     ,'SensitiveButton','True' )
          call pc_put_gui ('ProcessListRight','SensitiveButton','True' )
          call pc_put_gui ('ProjectDefault'  ,'SensitiveButton','False')
          call pc_put_gui ('UserDefault'     ,'SensitiveButton','False')
          call pc_put_gui ('Help'            ,'SensitiveButton','True' )
          call pc_put_gui ('PROCESSDEFAULTS' ,'Visible'        ,'False')
          call pc_put_gui ('MWBLABEL'        ,'Visible'        ,'True' )
          call pc_put_gui ('OK'              ,'Visible'        ,'False')
          call pc_put_gui ('Apply'           ,'Visible'        ,'False')
          call pc_put_gui ('Cancel'          ,'Visible'        ,'False')
          call pc_put_gui ('Reset'           ,'Visible'        ,'False')
          call pc_put_gui ('ProjectDefault'  ,'Visible'        ,'False')
          call pc_put_gui ('UserDefault'     ,'Visible'        ,'False')
          readonly_flag = .true.
      end select

      if (len_trim(process_name) .eq. 0) then
        xml_name = 'none.xml'
      else
        xml_name = trim(process_name) // '.xml'
      endif

      call string_to_lower (xml_name)
      call cfewindow_get_window_id (window,window_id)

      if (object%lnklib .eq. CFE_UNKNOWN) then
        if (object%ncustom_xml .gt. 0) then
          do i=1,object%ncustom_xml
!!!         j = index        (object%custom_xml(i),'/',.true.)
            j = string_index (object%custom_xml(i),'/',.true.)
            if (j .gt. 0) then
              if (trim(object%custom_xml(i)(j+1:)) .eq. trim(xml_name)) then
                xml_name = object%custom_xml(i)
                exit
              endif
            endif
          enddo
        endif
      endif

      if (pc_gui_action_present('PROCESS','WINDOWKEYS')) then
        extrakeys( 1) = 'OK'
        extrakeys( 2) = 'APPLY'
        extrakeys( 3) = 'CANCEL'
        extrakeys( 4) = 'RESET'
        extrakeys( 5) = 'PROJECTDEFAULT'
        extrakeys( 6) = 'USERDEFAULT'
        extrakeys( 7) = 'HELP'
        extrakeys( 8) = 'PROCESSDEFAULTS'
        extrakeys( 9) = 'PROCESSLIST'
        extrakeys(10) = 'PROCESSLISTLEFT'
        extrakeys(11) = 'PROCESSLISTRIGHT'
        extrakeys(12) = 'CLOSE'
        extrakeys(13) = 'ERROR'
        extrakeys(14) = 'WARNING'
        extrakeys(15) = 'INFO'
        nkeys = pc_num_elements_gui ('PROCESS','WINDOWKEYS')
        if (nkeys .gt. 0) then
          allocate(keys(nkeys+nextrakeys))
          call pc_get_gui ('PROCESS','WINDOWKEYS',keys,nkeys)
          do i=1,nkeys
            call string_to_upper (keys(i))
          enddo
          keys(nkeys+1:nkeys+nextrakeys) = extrakeys
          nkeys = nkeys + nextrakeys
          call cfewindow_set_keys (window,keys,nkeys)
          deallocate(keys)
        endif
      endif
      if (present(same_window)) then
        call cfegui_create (window_id,trim(window_title),trim(xml_name),  &
                            readonly_flag,same_window)
      else
        call cfegui_create (window_id,trim(window_title),trim(xml_name),  &
                            readonly_flag)
      endif

      call cfewindow_set_current (window)

      if (associated(cards)) deallocate(cards)

      return
      end subroutine cfeprocess_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine cfeprocess_delete (new_process,keyword)
      implicit none
      type(process_struct),pointer      :: new_process                 !argument
      character(len=*),intent(in)       :: keyword                     !argument

      character(len=PC_LENGTH)          :: local_key                   !local

      local_key = keyword
!     call string_to_upper (local_key)
      select case (trim(local_key))
        case ('CURRENTPROCESSLIST')
          call process_delete (new_process)
        case ('SUBSETPROCESSLIST')
          call process_delete (new_process)
      end select

      return
      end subroutine cfeprocess_delete


!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!


      subroutine cfeprocess_update (new_process,keyword,indx,err)
      implicit none
      type(process_struct),pointer      :: new_process                 !argument
      character(len=*),intent(in)       :: keyword                     !argument
      integer         ,intent(in)       :: indx                        !argument
      logical         ,intent(out)      :: err                         !argument

      type(process_struct),pointer      :: previous                    !local
      type(process_struct),pointer      :: next                        !local
      type(process_struct),pointer      :: old_process                 !local
      character(len=PC_LENGTH)          :: process_name                !local
      integer                           :: i                           !local
      integer                           :: nwd                         !local
      integer                           :: ncjn                        !local
      logical                           :: run_traps                   !local

      nullify (previous) ! jpa
      nullify (next) ! jpa
      nullify (old_process) ! jpa

      err = .false.

      call workfile_get_first_process (object%currentWorkfile,old_process)
      if (indx .gt. 1) then
        do i = 2, indx
          if (.not. associated(old_process)) exit
!!!       call process_get_next (old_process,old_process)
          call process_get_next (old_process,next)
          old_process => next
        enddo
      endif

      if (.not. associated(old_process)) then
        call pc_error ('Error updating process')
        return
      endif
      
      call process_get_previous      (old_process, previous)
      call process_copy_global_cards (previous, new_process)
      call process_update            (new_process, frontend=.true.,  &
                                      found_errors=err)
      call process_get_name (new_process,process_name)
      if (len_trim(process_name) .eq. 0) err = .false.
      if (err) return

      call process_get_name (old_process,process_name)
      if (trim(process_name) .eq. 'PROJECT_DATA' .or.  &
          trim(process_name) .eq. 'JOB_DATA') then
        run_traps = .true.
      else 
        if (process_compare_global(old_process,new_process)) then
          run_traps = .false.
        else
          run_traps = .true.
        endif
      endif

      call process_get_previous      (old_process, previous)
      call process_copy_pdata_cards  (previous   , old_process)
      call process_copy_jdata_cards  (previous   , old_process)
      call process_copy_global_cards (previous   , old_process)
      call process_copy              (new_process, old_process)
      if (run_traps) then
        call process_get_next (old_process,next)
        call workfile_run_all_traps(object%currentWorkfile,start=next,  &
                                    report_errors=.true.)
      endif

      call string_strip_blanks(object%working_dir,nwd)
      call string_strip_blanks(object%currentJobName,ncjn)

      call workfile_write (object%currentWorkfile,       &
                           object%working_dir(1:nwd) //  &
                           object%currentJobName(1:ncjn))

      call cfeprocess_update_all_popups()

      return
      end subroutine cfeprocess_update


!!-------------------------- update_all_popups -----------------------------!!
!!-------------------------- update_all_popups -----------------------------!!
!!-------------------------- update_all_popups -----------------------------!!


      subroutine cfeprocess_update_all_popups()
      implicit none

      integer                           :: i                           !local
      integer,pointer                   :: window_ids(:)               !local
      integer                           :: nwindow_ids                 !local
      integer                           :: current_id                  !local
      type(cfewindow_struct),pointer    :: window                      !local
      type(process_struct),pointer      :: popup_process               !local

      nullify (window_ids) ! jpa
      nullify (window) ! jpa

      call cfewindow_get_all_of_type ('PROCESS',window_ids)
      if (associated(window_ids)) then
        call cfewindow_get_current (window)
        call cfewindow_get_window_id (window,current_id)
        nwindow_ids = size(window_ids)
        call pc_put_gui ('PROCESS','UpdateWindow',window_ids,nwindow_ids)
        do i=1,nwindow_ids
          if (window_ids(i) .eq. current_id) cycle
          call cfewindow_get_pointer (window_ids(i),window)
          nullify(popup_process)
          call cfewindow_get_process (window,popup_process)
          call cfeprocess_update_popup (popup_process)
        enddo
        deallocate(window_ids)
      endif

      return
      end subroutine cfeprocess_update_all_popups


!!---------------------------- update_popup --------------------------------!!
!!---------------------------- update_popup --------------------------------!!
!!---------------------------- update_popup --------------------------------!!


      subroutine cfeprocess_update_popup (process)
      implicit none
      type(process_struct),pointer      :: process                     !argument

      character(len=CARDSET_LENGTH),pointer         :: errors(:)      ! local
      character(len=CARDSET_LENGTH),pointer         :: warnings(:)    ! local
      character(len=CARDSET_LENGTH),pointer         :: infos(:)       ! local
      integer                                       :: i              ! local
      integer                                       :: nerrors        ! local
      integer                                       :: nwarnings      ! local
      integer                                       :: ninfos         ! local

      nullify(errors)
      nullify(warnings)
      nullify(infos)

      call process_run_traps (process,errors,warnings,infos)

      if (associated(errors)) then
        nerrors = size(errors)
        call pc_put_gui ('ERROR','ERROR',errors,nerrors)
        do i=1,nerrors
          write(STDOUT,*) 'ERROR -  ',errors(i)
        enddo
        deallocate(errors)
      endif
 
      if (associated(warnings)) then
        nwarnings = size(warnings)
        call pc_put_gui ('WARNING','WARNING',warnings,nwarnings)
        do i=1,nwarnings
          write(STDOUT,*) 'WARNING -  ',warnings(i)
        enddo
        deallocate(warnings)
      endif
 
      if (associated(infos)) then
        ninfos = size(infos)
        call pc_put_gui ('INFO','INFO',infos,ninfos)
        do i=1,ninfos
          write(STDOUT,*) 'INFO -  ',infos(i)
        enddo
        deallocate(infos)
      endif

      return
      end subroutine cfeprocess_update_popup


!!-------------------------- delete_all_popups -----------------------------!!
!!-------------------------- delete_all_popups -----------------------------!!
!!-------------------------- delete_all_popups -----------------------------!!


      subroutine cfeprocess_delete_all_popups()
      implicit none

      integer,pointer                 :: window_ids(:)                 !local
      integer                         :: nwindow_ids                   !local
      integer                         :: i                             !local

      nullify(window_ids)
      call cfewindow_get_all_of_type ('PROCESS',window_ids)
      if (associated(window_ids)) then
        nwindow_ids = size(window_ids)
        do i=1,nwindow_ids
          call cfeprocess_delete_popup (window_ids(i))
        enddo
        deallocate(window_ids)
      endif

      return
      end subroutine cfeprocess_delete_all_popups


!!---------------------------- delete_popup --------------------------------!!
!!---------------------------- delete_popup --------------------------------!!
!!---------------------------- delete_popup --------------------------------!!


      subroutine cfeprocess_delete_popup (window_id)
      implicit none
      integer,intent(in)                :: window_id                   !argument

      type(process_struct),pointer      :: process                     !local
      type(cfewindow_struct),pointer    :: window                      !local

      nullify (process)
      nullify (window) ! jpa

      call cfegui_delete         (window_id,'Close')
      call cfewindow_get_pointer (window_id,window )
      call cfewindow_get_process (window   ,process)
      call process_delete        (process)
      call cfewindow_delete      (window)

      return
      end subroutine cfeprocess_delete_popup


!!-------------------------------- reset -----------------------------------!!
!!-------------------------------- reset -----------------------------------!!
!!-------------------------------- reset -----------------------------------!!


      subroutine cfeprocess_reset (new_process,keyword,indx)
      implicit none
      type(process_struct),pointer      :: new_process                 !argument
      character(len=*),intent(in)       :: keyword                     !argument
      integer         ,intent(in)       :: indx                        !argument

      type(process_struct),pointer      :: previous,next               !local
      character(len=PC_LENGTH)          :: local_key                   !local
      type(process_struct),pointer      :: old_process                 !local
      integer                           :: i                           !local
      character(len=PC_LENGTH),pointer  :: cards(:)                    !local
      integer                           :: ncards                      !local

      nullify (previous) ! jpa
      nullify (next) ! jpa
      
      local_key = keyword
!     call string_to_upper (local_key)
      if (trim(local_key) .ne. 'CURRENTPROCESSLIST') return

      call workfile_get_first_process (object%currentWorkfile,old_process)
      if (indx .gt. 1) then
        do i = 2, indx
          if (.not. associated(old_process)) exit
!!!       call process_get_next (old_process,old_process)
          call process_get_next (old_process,next)
          old_process => next
        enddo
      endif

      if (.not. associated(old_process)) then
        call pc_error ('Error updating process')
        return
      endif
      
      call process_get_previous      (old_process, previous)
      call process_copy_pdata_cards  (previous   , new_process)
      call process_copy_jdata_cards  (previous   , new_process)
      call process_copy_global_cards (previous   , new_process)
      call process_copy              (old_process, new_process)
      nullify(cards)
      call process_alloc_gui_cards (new_process, cards, ncards)
      call pc_put_gui_cards (cards, ncards)
      if (associated(cards)) deallocate (cards)

      return
      end subroutine cfeprocess_reset


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module cfeprocess_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

