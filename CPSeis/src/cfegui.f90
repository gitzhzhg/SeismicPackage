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
! Name       : cfegui 
! Category   : cfe
! Written    : 1999-09-07   by: Donna K. Vunderink
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : GUI Interface Module.
! Portability: No known limitations.
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
!025. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
! 24. 2006-01-10  B. Menger    Removed Unused Variables.
! 23. 2003-09-15  Stoeckley    Changed name from cfe_gui to cfegui; remove
!                                restriction about inserting a line at the
!                                end of an array, since this is legitimate
!                                after a line has been deleted; changed type
!                                to primitive; changed category to cfe; changed
!                                names of called primitives as necessary.
! 22. 2002-05-29  Vunderink    Transfer OPTIONSARRAY first like OPTIONSFILED.
! 21. 2002-05-02  Vunderink    Fix insert_array_element to send error message
!                                if indx is not valid.
! 20. 2002-01-04  Vunderink    Modified cfegui_transfer for new buildlist
!                                window.
! 19. 2001-06-11  Vunderink    Do not send FILEBOX window parameters to parent
! 18. 2001-03-12  Vunderink    Transfer OPTIONSFIELD action first and mark to
!                                refresh default
! 17. 2000-12-05  Vunderink    Added check in transfer_private for BEEP action
! 16. 2000-10-11  Vunderink    Modified cfegui_modify_field to see is last
!                                action was MODIFYFIELD with same keyword.  This
!                                is to still get around INT software problem and
!                                to allow reselection of comboboxes.
! 15. 2000-09-04  Vunderink    Eliminate un-necessary calls to string_to_upper
!                                and some memory leaks.
! 14. 2000-08-21  Vunderink    Removed un-necessary string_to_upper calls
! 13. 2000-08-15  Vunderink    Changed character variable back to PC_LENGTH,
!                                changed cfe_api calls to int_api calls, and
!                                enhanced for multi-workfilebuilder
! 12. 2000-07-21  Vunderink    Update cardset when sending ClearElements
! 11. 2000-05-30  Vunderink    If action is "SensitiveButton" change to
!                                "SensitiveField".  If window OLDPROCESS or
!                                LISTPROCESS, do not transfer sensitivity unless
!                                "SensitiveButton".
! 10. 2000-05-23  Vunderink    Added support for new parameter cache actions
!                                MINSIZEARRAY, MAXSIZEARRAY, MINSIZEARRAYSET and
!                                MAXSIZEARRAYSET and adding an extra row to
!                                arrays.
!  9. 2000-05-09  Vunderink    Modified cfegui_modify_field to compare value
!                                to that in the cardset and only insert into
!                                the parameter cache if the value has changed.
!                                This is for the MODIFYFIELD action only and
!                                is to get around problem with the INT software.
!  8. 2000-04-24  Vunderink    Added main_window_id, cfegui_set_main_window and
!                                cfegui_jump_field and changed cfegui_beep and
!                                cfe_clear_selections to use main_window_id
!  7. 2000-04-24  Vunderink    Changed character variables to use named constant
!                                CFE_LENGTH, and made changes required for color
!                                background on WorkfileBuilder arrays.
!  6. 2000-03-16  Vunderink    Fixed cfegui_transfer_private to always send
!                                SelectRadioButton messages across API.
!  5. 2000-03-08  Vunderink    Move memory deallocation to end of routine
!                                cfegui_transfer_private to avoid leak.
!  4. 2000-02-29  Vunderink    Added cfegui_update_window, moved 
!                                cfegui_transfer to cfegui_transfer_private,
!                                and added new cfegui_transfer that will update
!                                process popup windows.
!                                Added cfegui_clientversion
!  3. 2000-02-16  Vunderink    Added cfegui_title
!  2. 2000-01-31  Vunderink    Fixed bug in cfegui_remove_array_element when
!                                deleting row in arrayset
!  1. 1999-09-07  Vunderink    Initial version.
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module cfegui_module
      use pc_module
      use string_module
      use cfewindow_module
      use cardset_module

      implicit none

      private
      public :: cfegui_create
      public :: cfegui_delete
      public :: cfegui_title
      public :: cfegui_set_main_window
      public :: cfegui_beep
      public :: cfegui_clear_selection
      public :: cfegui_jump_field
      public :: cfegui_update_window
      public :: cfegui_showhelp
      public :: cfegui_clientversion
      public :: cfegui_transfer
      public :: cfegui_new_window
      public :: cfegui_jump_window
      public :: cfegui_modify_field
      public :: cfegui_modify_array_element
      public :: cfegui_insert_array_element
      public :: cfegui_remove_array_element
      public :: cfegui_save_arrayset_elements
      public :: cfegui_insert_allowed
      public :: cfegui_remove_allowed

      character(len=100),public,save :: cfegui_ident = &
       '$Id: cfegui.f90,v 1.25 2006/09/18 13:32:41 Glover prod sps $'

      integer           ,public,save :: main_window_id =  1
      integer,parameter,private      :: NO_MINSIZE     = -1
      integer,parameter,private      :: NO_MAXSIZE     = -1

      contains


!!------------------------------ create -----------------------------------!!
!!------------------------------ create -----------------------------------!!
!!------------------------------ create -----------------------------------!!


      subroutine cfegui_create (window_id,keyword,value,readonly_flag,  &
                                existing_window)

      integer,intent(in)              :: window_id                     !argument
      character(len=*),intent(in)     :: keyword                       !argument
      character(len=*),intent(in)     :: value                         !argument
      logical                         :: readonly_flag                 !argument
      logical,optional                :: existing_window               !argument

      character(len=PC_LENGTH)        :: action                        !local
      integer                         :: h_action(PC_LENGTH)            !local
      integer                         :: h_keyword(PC_LENGTH)           !local
      integer                         :: h_value(PC_LENGTH)             !local
      character(len=PC_LENGTH)        :: local_readonly                !local


      if (present(existing_window)) then
        if (existing_window) then
          action = 'ReplaceGUI'
        else
          action = 'SetGUI'
        endif
      else
        action  = 'SetGUI'
      endif
      call string_cc2hh(action , h_action)
      call string_cc2hh(value  , h_value)
      if (readonly_flag) then
        local_readonly = 'true'
      else
        local_readonly = 'false'
      endif
      call string_cc2hh(local_readonly, h_keyword)
      call int_api_put_value (window_id, h_action, h_keyword, h_value)

      action  = 'SetWindowTitle'
      call string_cc2hh(action , h_action)
      call string_cc2hh(keyword, h_keyword)
      call int_api_put_value (window_id, h_action, h_keyword, h_keyword)

      return
      end subroutine cfegui_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine cfegui_delete (window_id,action)

      integer         ,intent(in)     :: window_id                     !argument
      character(len=*),intent(in)     :: action                        !argument

      character(len=PC_LENGTH)        :: keyword                       !local
      character(len=PC_LENGTH)        :: value                         !local
      integer                         :: h_action(PC_LENGTH)           !local
      integer                         :: h_keyword(PC_LENGTH)          !local
      integer                         :: h_value(PC_LENGTH)            !local

      keyword = '    '
      value   = '    '
      call string_cc2hh(action , h_action)
      call string_cc2hh(keyword, h_keyword)
      call string_cc2hh(value  , h_value)
      call int_api_put_value (window_id, h_action, h_keyword, h_value)

      return
      end subroutine cfegui_delete


!!------------------------------- title -----------------------------------!!
!!------------------------------- title -----------------------------------!!
!!------------------------------- title -----------------------------------!!


      subroutine cfegui_title (window_id,title)

      integer,intent(in)              :: window_id                     !argument
      character(len=*),intent(in)     :: title                         !argument

      character(len=PC_LENGTH)        :: action                        !local
      integer                         :: h_action(PC_LENGTH)           !local
      integer                         :: h_keyword(PC_LENGTH)          !local


      action  = 'SetWindowTitle'
      call string_cc2hh(action , h_action)
      call string_cc2hh(title  , h_keyword)
      call int_api_put_value (window_id, h_action, h_keyword, h_keyword)

      return
      end subroutine cfegui_title


!!-------------------------- set_main_window ------------------------------!!
!!-------------------------- set_main_window ------------------------------!!
!!-------------------------- set_main_window ------------------------------!!


      subroutine cfegui_set_main_window (window_id)

      integer,intent(in)              :: window_id                     !argument

      main_window_id = window_id

      return
      end subroutine cfegui_set_main_window


!!-------------------------------- beep -----------------------------------!!
!!-------------------------------- beep -----------------------------------!!
!!-------------------------------- beep -----------------------------------!!


      subroutine cfegui_beep ()

      character(len=PC_LENGTH)        :: action                        !local
      character(len=PC_LENGTH)        :: keyword                       !local
      character(len=PC_LENGTH)        :: value                         !local
      integer                         :: h_action(PC_LENGTH)           !local
      integer                         :: h_keyword(PC_LENGTH)          !local
      integer                         :: h_value(PC_LENGTH)            !local

      action  = 'Beep'
      keyword = '    '
      value   = '    '
      call string_cc2hh(action , h_action)
      call string_cc2hh(keyword, h_keyword)
      call string_cc2hh(value  , h_value)
      call int_api_put_value (main_window_id, h_action, h_keyword, h_value)

      return
      end subroutine cfegui_beep


!!--------------------------- clear_selection -----------------------------!!
!!--------------------------- clear_selection -----------------------------!!
!!--------------------------- clear_selection -----------------------------!!


      subroutine cfegui_clear_selection (keyword)

      character(len=*)                :: keyword                       !argument

      character(len=PC_LENGTH)        :: action                        !local
      character(len=PC_LENGTH)        :: value                         !local
      integer                         :: h_action(PC_LENGTH)           !local
      integer                         :: h_keyword(PC_LENGTH)          !local
      integer                         :: h_value(PC_LENGTH)            !local

      action = 'ClearSelection'
      value  = '    '

      call string_cc2hh(action , h_action )
      call string_cc2hh(keyword, h_keyword)
      call string_cc2hh(value  , h_value  )

      call int_api_put_value (main_window_id, h_action, h_keyword, h_value)

      return
      end subroutine cfegui_clear_selection


!!----------------------------- jump_field --------------------------------!!
!!----------------------------- jump_field --------------------------------!!
!!----------------------------- jump_field --------------------------------!!


      subroutine cfegui_jump_field (keyword)

      character(len=*)                :: keyword                       !argument

      character(len=PC_LENGTH)        :: action                        !local
      character(len=PC_LENGTH)        :: value                         !local
      integer                         :: h_action(PC_LENGTH)           !local
      integer                         :: h_keyword(PC_LENGTH)          !local
      integer                         :: h_value(PC_LENGTH)            !local

      action = 'JumpField'
      value  = '    '

      call string_cc2hh(action , h_action )
      call string_cc2hh(keyword, h_keyword)
      call string_cc2hh(value  , h_value  )

      call int_api_put_value (main_window_id, h_action, h_keyword, h_value)

      return
      end subroutine cfegui_jump_field


!!-------------------------- clientversion --------------------------------!!
!!-------------------------- clientversion --------------------------------!!
!!-------------------------- clientversion --------------------------------!!


      subroutine cfegui_clientversion()

      character(len=PC_LENGTH)        :: action                        !local
      character(len=PC_LENGTH)        :: keyword                       !local
      character(len=PC_LENGTH)        :: value                         !local
      integer                         :: h_action(PC_LENGTH)           !local
      integer                         :: h_keyword(PC_LENGTH)          !local
      integer                         :: h_value(PC_LENGTH)            !local

      action  = 'ClientVersion'
      keyword = '    '
      value   = '    '
      call string_cc2hh(action , h_action)
      call string_cc2hh(keyword, h_keyword)
      call string_cc2hh(value  , h_value)
      call int_api_put_value (VERSION_WINDOW, h_action, h_keyword, h_value)

      return
      end subroutine cfegui_clientversion


!!----------------------------- showhelp ----------------------------------!!
!!----------------------------- showhelp ----------------------------------!!
!!----------------------------- showhelp ----------------------------------!!


      subroutine cfegui_showhelp()

      character(len=PC_LENGTH)        :: action                        !local
      character(len=PC_LENGTH)        :: keyword                       !local
      character(len=PC_LENGTH)        :: value                         !local
      integer                         :: h_action(PC_LENGTH)           !local
      integer                         :: h_keyword(PC_LENGTH)          !local
      integer                         :: h_value(PC_LENGTH)            !local

      action  = 'ShowHelp'
      keyword = '    '
      value   = 'cfe_apphelp.xml'
      call string_cc2hh(action , h_action)
      call string_cc2hh(keyword, h_keyword)
      call string_cc2hh(value  , h_value)
      call int_api_put_value (HELP_WINDOW, h_action, h_keyword, h_value)

      return
      end subroutine cfegui_showhelp


!!--------------------------- update_window ------------------------------!!
!!--------------------------- update_window ------------------------------!!
!!--------------------------- update_window ------------------------------!!


      subroutine cfegui_update_window (window_ids,nwindow_ids)
      implicit none
      integer,pointer                          :: window_ids(:)        !argument
      integer,intent(out)                      :: nwindow_ids          !argument

      integer                                  :: i                    !local
      integer                                  :: num_keywords         !local
      integer                                  :: nature               !local
      character(len=PC_LENGTH)                 :: action               !local
      character(len=PC_LENGTH)                 :: keyword              !local



      if (associated(window_ids)) deallocate(window_ids)
      nwindow_ids = 0

      num_keywords = pc_num_gui_keywords()
      do i=1,num_keywords
        keyword = pc_get_gui_keyword(i)
        action = pc_get_gui_action(i)
        if (trim(action) .ne. 'UPDATEWINDOW') cycle
        nature  = pc_nature_gui(keyword,trim(action))
        if (nature .eq. PC_ARRAY) then
          nwindow_ids = pc_num_elements_gui(keyword,trim(action))
          if (nwindow_ids .gt. 0) then
            call pc_alloc_gui(keyword,trim(action),window_ids,nwindow_ids)
          endif
        endif
      enddo

      return
      end subroutine cfegui_update_window


!!---------------------------- new_window --------------------------------!!
!!---------------------------- new_window --------------------------------!!
!!---------------------------- new_window --------------------------------!!


      subroutine cfegui_new_window (window)
      implicit none
      type(cfewindow_struct),pointer          :: window               !argument

      integer                                 :: i                    !local
      integer                                 :: j                    !local
      integer                                 :: num_keywords         !local
      integer                                 :: nature               !local
      integer                                 :: window_id            !local
      integer                                 :: nkeys                !local
      character(len=PC_LENGTH)                :: action               !local
      character(len=PC_LENGTH)                :: keyword              !local
      character(len=PC_LENGTH)                :: value                !local
      character(len=PC_LENGTH)                :: window_type          !local
      character(len=PC_LENGTH) ,pointer       :: keys(:)              !local
      type(cfewindow_struct),pointer          :: new_window           !local


      nullify(new_window)
      nullify(keys)
      nkeys = 0

      num_keywords = pc_num_gui_keywords()
      do i=1,num_keywords
        keyword = pc_get_gui_keyword(i)
        action = pc_get_gui_action(i)
        if (trim(action) .ne. 'WINDOWKEYS') cycle
        nature  = pc_nature_gui(keyword,trim(action))
        if (nature .eq. PC_ARRAY) then
          nkeys = pc_num_elements_gui(keyword,trim(action))
          if (nkeys .gt. 0) then
            call pc_alloc_gui(keyword,trim(action),keys,nkeys)
            do j=1,nkeys
              call string_to_upper (keys(j))
            enddo
          endif
        endif
      enddo
      do i=1,num_keywords
        keyword = pc_get_gui_keyword(i)
        action = pc_get_gui_action(i)
        if (trim(action) .ne. 'NEWWINDOW') cycle
        nature  = pc_nature_gui(keyword,trim(action))
        if (nature .eq. PC_SCALAR) then
          call pc_get_gui(keyword,trim(action),value)
          call cfewindow_create_child  (window     ,new_window)
          call cfewindow_set_keyword   (new_window ,keyword)
          call cfewindow_set_value     (new_window ,value)
          if (pc_gui_action_present(keyword,'WINDOWTYPE')) then
            call pc_get_gui(keyword,'WINDOWTYPE',window_type)
            call cfewindow_set_window_type (new_window,trim(window_type))
          endif
          call cfewindow_set_keys      (new_window ,keys, nkeys)
          call cfewindow_get_window_id (new_window ,window_id)
          call cfegui_create (window_id,trim(keyword),trim(value),.false.)
          call cfewindow_set_current (new_window)
        endif
      enddo

      if (associated(keys)) deallocate(keys)

      return
      end subroutine cfegui_new_window


!!---------------------------- jump_window -------------------------------!!
!!---------------------------- jump_window -------------------------------!!
!!---------------------------- jump_window -------------------------------!!


      subroutine cfegui_jump_window (window_id)
      implicit none

      integer                                 :: window_id            !argument

      type(cfewindow_struct),pointer          :: window               !local
      character(len=PC_LENGTH)                :: action               !local
      character(len=PC_LENGTH)                :: keyword              !local
      character(len=PC_LENGTH)                :: value                !local
      integer                                 :: h_action(PC_LENGTH)  !local
      integer                                 :: h_keyword(PC_LENGTH)  !local
      integer                                 :: h_value(PC_LENGTH)   !local

      nullify(window)

      call cfewindow_get_pointer (window_id, window)
      call cfewindow_set_current (window)

      action  = 'JumpWindow'
      keyword = '    '
      value   = '    '
      call string_cc2hh(action , h_action )
      call string_cc2hh(keyword, h_keyword)
      call string_cc2hh(value  , h_value  )
      call int_api_put_value (window_id, h_action, h_keyword, h_keyword)

      return
      end subroutine cfegui_jump_window


!!----------------------------- transfer ---------------------------------!!
!!----------------------------- transfer ---------------------------------!!
!!----------------------------- transfer ---------------------------------!!


      subroutine cfegui_transfer (window_current)
      implicit none
      type(cfewindow_struct),pointer          :: window_current       !argument

      integer                                 :: i                    !local
      integer                                 :: nwindow_ids          !local
      integer,pointer                         :: window_ids(:)        !local
      type(cfewindow_struct),pointer          :: window               !local

      nullify (window_ids)
      nullify (window_ids) ! jpa

      call cfegui_transfer_private (window_current)
      call cfegui_update_window (window_ids,nwindow_ids)
      if (nwindow_ids .gt. 0) then
        do i=1,nwindow_ids
          call cfewindow_get_pointer   (window_ids(i),window)
          call cfewindow_update        (window)
          call cfegui_transfer_private (window)
          call pc_clear_gui_cards
        enddo
      endif
      if (associated(window_ids))      deallocate(window_ids)

      return
      end subroutine cfegui_transfer


!!------------------------- transfer_private -----------------------------!!
!!------------------------- transfer_private -----------------------------!!
!!------------------------- transfer_private -----------------------------!!


      subroutine cfegui_transfer_private (window_current)
      implicit none
      type(cfewindow_struct),pointer          :: window_current       !argument

      type(cardset_struct),pointer            :: cardset              !local
      character(len=PC_LENGTH)                :: action               !local
      character(len=PC_LENGTH)                :: keyword              !local
      character(len=PC_LENGTH)                :: value                !local
      character(len=PC_LENGTH) ,pointer       :: array(:)             !local
      character(len=PC_LENGTH) ,pointer       :: keys(:)              !local
      character(len=PC_LENGTH) ,pointer       :: refresh(:)           !local
      character(len=PC_LENGTH)                :: bigword              !local
      character(len=PC_LENGTH)                :: extrarow             !local
      character(len=PC_LENGTH)                :: window_type          !local
      character(len=PC_LENGTH) ,allocatable   :: temp_array(:)        !local
      integer                                 :: i                    !local
      integer                                 :: j                    !local
      integer                                 :: j1                   !local
      integer                                 :: j2                   !local
      integer                                 :: nbpw                 !local
      integer                                 :: nwpe                 !local
      integer                                 :: nature               !local
      integer                                 :: narray               !local
      integer                                 :: nkeys                !local
      integer                                 :: nrefresh             !local
      integer                                 :: num_keywords         !local
      integer                                 :: naction              !local
      integer                                 :: h_action(PC_LENGTH)  !local
      integer                                 :: h_keyword(PC_LENGTH) !local
      integer                                 :: h_value(PC_LENGTH)   !local
      integer,pointer                         :: h_array(:)           !local
      integer                                 :: window_id            !local
      integer                                 :: maxsize              !local
      integer                                 :: minsize              !local
      logical                                 :: add_extrarow         !local
      logical                                 :: found_key            !local
      type(cfewindow_struct),pointer          :: window               !local
      type(cfewindow_struct),pointer          :: parent               !local


      if (.not. associated(window_current)) return

      window => window_current
      nullify(cardset)
      nullify(array)
      nullify(keys)
      nullify(refresh)
      nullify(h_array)
      nullify(parent)

      call cfegui_alloc_refresh_keywords (refresh,nrefresh)

      do
        if (.not. associated(window)) exit
        call cfewindow_get_cardset     (window,cardset)
        call cfewindow_get_window_id   (window,window_id)
        call cfewindow_get_window_type (window,window_type)
        call cfewindow_alloc_keys      (window,keys,nkeys)
        call cfewindow_get_parent      (window,parent)

        nbpw = bit_size(i) / 8                       ! # bytes per word
        nwpe = PC_LENGTH/nbpw                  ! # words per array element
        num_keywords = pc_num_gui_keywords()

        do i=1,num_keywords
          keyword = pc_get_gui_keyword(i)
          action = pc_get_gui_action(i)
          if (trim(action) .ne. 'OPTIONSFIELD' .and.  &
              trim(action) .ne. 'OPTIONSARRAY') cycle
          if (nkeys .gt. 0) then
            found_key = .false.
            do j=1,nkeys
              if (trim(keys(j)) .eq. trim(keyword)) then
                found_key = .true.
                exit
              endif
            enddo
            if (.not. found_key) cycle
          endif
          if (trim(window_type) .ne. 'FILEBOX') then
            if (trim(keyword) .eq. 'FILEBOX_FILTER'      .or.  &
                trim(keyword) .eq. 'FILEBOX_DIRECTORIES' .or.  &
                trim(keyword) .eq. 'FILEBOX_FILES'       .or.  &
                trim(keyword) .eq. 'FILEBOX_SELECTION'   .or.  &
                trim(keyword) .eq. 'FILEBOX_FILTERBUTTON') cycle
          endif
          naction = len_trim(action)
          nature  = pc_nature_gui(keyword,action(1:naction))
          bigword = trim(keyword)//'#'//action
          if (nature .eq. PC_ARRAY) then
            if (.not. associated(cardset)) then
              call cardset_create (cardset)
              call cfewindow_set_cardset (window,cardset)
            endif
            narray = pc_num_elements_gui(keyword,action(1:naction))
            if (narray .gt. 0) then
              call string_cc2hh(action , h_action)
              call string_cc2hh(keyword, h_keyword)
              if (associated(array)) deallocate(array)
              allocate(array(narray))
              call pc_get_gui(keyword,action(1:naction),array,narray)
              if (cardset_array_matches(cardset,bigword,array,narray)) then
                cycle
              else
                call cardset_put_array(cardset,bigword,array,narray)
              endif
              if (associated(refresh)) then
                allocate(temp_array(nrefresh))
                temp_array(1:nrefresh) = refresh(1:nrefresh)
                deallocate(refresh)
                allocate(refresh(nrefresh+1))
                refresh(1:nrefresh) = temp_array(1:nrefresh)
                deallocate(temp_array)
              else
                allocate(refresh(nrefresh+1))
              endif
              nrefresh = nrefresh + 1
              refresh(nrefresh) = trim(keyword)
              if (associated(h_array)) deallocate(h_array)
              allocate(h_array(nwpe*narray))
              do j=1,narray
                j1 = PC_LENGTH*(j-1)/nbpw+1
                j2 = j1+PC_LENGTH/nbpw-1
                call string_cc2hh(array(j),h_array(j1:j2))
              enddo
              call int_api_put_array(window_id,h_action,h_keyword,h_array,   &
                                     narray, PC_LENGTH, 1, narray)
            endif
          endif
        enddo

        do i=1,num_keywords
          keyword = pc_get_gui_keyword(i)
          action = pc_get_gui_action(i)
          naction = len_trim(action)
          if (action(1:naction) .eq. 'BUTTONPRESS'    ) cycle
          if (action(1:naction) .eq. 'ENTERWINDOW'    ) cycle
          if (action(1:naction) .eq. 'ITEMCLICKED'    ) cycle
          if (action(1:naction) .eq. 'ITEMSELECTED'   ) cycle
          if (action(1:naction) .eq. 'JUMPWINDOW'     ) cycle
          if (action(1:naction) .eq. 'LEAVEARRAY'     ) cycle
          if (action(1:naction) .eq. 'LEAVEARRAYSET'  ) cycle
          if (action(1:naction) .eq. 'LEAVESCREEN'    ) cycle
          if (action(1:naction) .eq. 'NEWWINDOW'      ) cycle
          if (action(1:naction) .eq. 'OPTIONSFIELD'   ) cycle
          if (action(1:naction) .eq. 'OPTIONSARRAY'   ) cycle
          if (action(1:naction) .eq. 'UNKNOWN'        ) cycle
          if (action(1:naction) .eq. 'UPDATEWINDOW'   ) cycle
          if (action(1:naction) .eq. 'WINDOWKEYWORD'  ) cycle

          if (action(1:naction) .eq. 'BEEP'           ) then
            call cfegui_beep()
            cycle
          endif

          if (trim(window_type) .eq. 'OLDPROCESS'   .or.   &
              trim(window_type) .eq. 'LISTPROCESS'  .or.   &
              trim(window_type) .eq. 'TEMPLATEPROCESS') then
            if (action(1:naction) .eq. 'SENSITIVEARRAY'   ) cycle
            if (action(1:naction) .eq. 'SENSITIVEARRAYSET') cycle
            if (action(1:naction) .eq. 'SENSITIVEFIELD'   ) cycle
          endif

          if (nkeys .gt. 0) then
            found_key = .false.
            do j=1,nkeys
              if (trim(keys(j)) .eq. trim(keyword)) then
                found_key = .true.
                exit
              endif
            enddo
            if (.not. found_key) cycle
          endif
          if (trim(window_type) .ne. 'FILEBOX') then
            if (trim(keyword) .eq. 'FILEBOX_FILTER'      .or.  &
                trim(keyword) .eq. 'FILEBOX_DIRECTORIES' .or.  &
                trim(keyword) .eq. 'FILEBOX_FILES'       .or.  &
                trim(keyword) .eq. 'FILEBOX_SELECTION'   .or.  &
                trim(keyword) .eq. 'FILEBOX_FILTERBUTTON') cycle
          endif
          nature  = pc_nature_gui(keyword,action(1:naction))
          bigword = trim(keyword)//'#'//action
!         call string_to_upper (bigword)
          if (nature .eq. PC_SCALAR) then
            call pc_get_gui(keyword,action(1:naction),value)
            if (.not. associated(cardset)) then
              call cardset_create (cardset)
              call cfewindow_set_cardset (window,cardset)
            endif
            select case (trim(action))
              case ('SELECTRADIOBUTTON','JUMPWINDOW'  ,'JUMPSCREEN',       &
                    'JUMPARRAYSETROW'  ,'JUMPARRAYSET','JUMPARRAYELEMENT', &
                    'JUMPARRAY'        ,'JUMPFIELD'   ,'CLEARSELECTION',   &
                    'SETARRAYBACKGROUNDCOLOR')
                call cardset_put_scalar(cardset,bigword,value)
              case ('MAXSIZEARRAY'     ,'MINSIZEARRAY','MAXSIZEARRAYSET',  &
                    'MINSIZEARRAYSET') 
                call cardset_put_scalar(cardset,bigword,value)
                cycle
              case default
                if (cfegui_keyword_matches (keyword,refresh,nrefresh)) then
                  call cardset_put_scalar(cardset,bigword,value)
                else if (cardset_scalar_matches(cardset,bigword,value)) then
                  cycle
                else
                  call cardset_put_scalar(cardset,bigword,value)
                endif
            end select
            if (action(1:naction).eq.'SENSITIVEBUTTON') action='SENSITIVEFIELD'
            call string_cc2hh(action , h_action)
            call string_cc2hh(keyword, h_keyword)
            call string_cc2hh(value  , h_value)
            call int_api_put_value (window_id,h_action,h_keyword,h_value)

          else if (nature .eq. PC_ARRAY) then
            narray = pc_num_elements_gui(keyword,action(1:naction))
            if (trim(action) .eq. 'REPLACEELEMENTS') then
              call cfegui_remove_array_size (keyword,cardset)
            endif
            if (narray .gt. 0) then
              call string_cc2hh(action , h_action)
              call string_cc2hh(keyword, h_keyword)
              if (trim(action) .eq. 'REPLACEELEMENTS') then
                add_extrarow = .true.
                call cfegui_get_array_maxsize (keyword,maxsize)
                if (maxsize .ne. NO_MAXSIZE) then
                  if (narray .ge. maxsize) add_extrarow = .false.
                endif
              else
                add_extrarow = .false.
              endif
              extrarow = trim(keyword)//'#EXTRAROW'
              if (add_extrarow) then
                if (associated(array)) deallocate(array)
                allocate(array(narray+1))
                call pc_get_gui(keyword,action(1:naction),array,narray)
                array(narray+1) = ' '
                narray = narray+1
              else
                call cardset_remove_keyword (cardset,extrarow)
                if (associated(array)) deallocate(array)
                allocate(array(narray))
                call pc_get_gui(keyword,action(1:naction),array,narray)
              endif
              if (.not. associated(cardset)) then
                call cardset_create (cardset)
                call cfewindow_set_cardset (window,cardset)
              endif
              select case (trim(action))
               case ('ERROR','WARNING','INFO')
                call cardset_put_array(cardset,bigword,array,narray)
               case ('ARRAYNAMES')
                call cardset_put_array(cardset,bigword,array,narray)
                cycle
               case default
                if (cfegui_keyword_matches (keyword,refresh,nrefresh)) then
                  if (add_extrarow) call cardset_put_scalar(cardset,extrarow,  &
                                                        keyword)
                  call cardset_put_array(cardset,bigword,array,narray)
                else if (cardset_array_matches(cardset,bigword,array,narray)) &
                                                                            then
                  cycle
                else
                  if (add_extrarow) call cardset_put_scalar(cardset,extrarow,  &
                                                        keyword)
                  call cardset_put_array(cardset,bigword,array,narray)
                endif
              end select
              if (associated(h_array)) deallocate(h_array)
              allocate(h_array(nwpe*narray))
              do j=1,narray
                j1 = PC_LENGTH*(j-1)/nbpw+1
                j2 = j1+PC_LENGTH/nbpw-1
                call string_cc2hh(array(j),h_array(j1:j2))
              enddo
              call int_api_put_array(window_id,h_action,h_keyword,h_array,   &
                                     narray, PC_LENGTH, 1, narray)

            else
              add_extrarow = .true.
              call cfegui_get_array_minsize (keyword,minsize)
              if (minsize .ne. NO_MINSIZE) then
                if (minsize .eq. 0) add_extrarow = .false.
              endif
              if (add_extrarow) then
                if (associated(array)) deallocate(array)
                allocate(array(narray+1))
                array(narray+1) = ' '
                narray = narray+1
                extrarow = trim(keyword)//'#EXTRAROW'
                if (associated(cardset)) then
                  if (cardset_array_matches(cardset,bigword,array,narray)) &
                      then
                    cycle
                  else
                    if (add_extrarow) call cardset_put_scalar(cardset,extrarow,&
                                                          keyword)
                    call cardset_put_array(cardset,bigword,array,narray)
                  endif
                else
                  call cardset_create (cardset)
                  call cfewindow_set_cardset (window,cardset)
                  if (add_extrarow) call cardset_put_scalar(cardset,extrarow,  &
                                                        keyword)
                  call cardset_put_array(cardset,bigword,array,narray)
                endif
                call string_cc2hh(action , h_action)
                call string_cc2hh(keyword, h_keyword)
                if (associated(h_array)) deallocate(h_array)
                allocate(h_array(nwpe*narray))
                do j=1,narray
                  j1 = PC_LENGTH*(j-1)/nbpw+1
                  j2 = j1+PC_LENGTH/nbpw-1
                  call string_cc2hh(array(j),h_array(j1:j2))
                enddo
                call int_api_put_array(window_id,h_action,h_keyword,h_array, &
                                       narray, PC_LENGTH, 1, narray)
              else
                add_extrarow = .false.
                if (associated(cardset)) then
                  if (cardset_array_matches(cardset,bigword,array,narray)) &
                      then
                    cycle
                  else
                    call cardset_put_array(cardset,bigword,array,narray)
                  endif
                else
                  call cardset_create (cardset)
                  call cfewindow_set_cardset (window,cardset)
                  call cardset_put_array(cardset,bigword,array,narray)
                endif
                action = 'ClearElements'
                value  = '    '
                call string_cc2hh(action , h_action)
                call string_cc2hh(keyword, h_keyword)
                call string_cc2hh(value  , h_value)
                call int_api_put_value (window_id,h_action,h_keyword,h_value)
              endif
            endif
          endif
        enddo

        if (associated(keys)) deallocate(keys)
        call cfewindow_get_window_type (parent,window_type)
        if (trim(window_type) .eq. 'PROCESS') then
          window => parent
        else
          nullify(window)
        endif
      enddo

      if (associated(array))   deallocate(array)
      if (associated(keys))    deallocate(keys)
      if (associated(refresh)) deallocate(refresh)
      if (associated(h_array)) deallocate(h_array)

      return
      end subroutine cfegui_transfer_private


!!--------------------------- modify_field --------------------------------!!
!!--------------------------- modify_field --------------------------------!!
!!--------------------------- modify_field --------------------------------!!


      subroutine cfegui_modify_field (window,keyword,action,value,  &
                                       last_keyword,last_action)

      implicit none
      type(cfewindow_struct),pointer          :: window               !argument
      character(len=*),intent(in)             :: keyword              !argument
      character(len=*),intent(in)             :: action               !argument
      character(len=*),intent(in)             :: value                !argument
      character(len=*),intent(in)             :: last_keyword         !argument
      character(len=*),intent(in)             :: last_action          !argument

      type(cardset_struct),pointer            :: cardset              !local
      character(len=PC_LENGTH)                :: bigword              !local
      character(len=PC_LENGTH)                :: saved                !local
      character(len=PC_LENGTH)                :: errmsg               !local

      if (.not. associated(window)) return

      nullify (cardset) ! jpa
      call cfewindow_get_cardset (window,cardset)

      bigword = trim(keyword)//'#'//trim(action)
      if (.not. associated(cardset)) then
        call cardset_create (cardset)
        call cfewindow_set_cardset (window,cardset)
      endif
      if (trim(action)     .eq.'MODIFYFIELD' .and.  &
          trim(last_action).eq.'MODIFYFIELD') then
        if (cardset_keyword_present(cardset,bigword)) then
          call cardset_get_scalar(cardset,bigword,saved,errmsg)
          if (trim(last_keyword).ne.trim(keyword) .or.  &
              trim(value).ne.trim(saved)) then
            call cardset_put_scalar (cardset,bigword,value)
            call pc_put_gui (keyword, action, value)
          endif
        endif
      else
        call cardset_put_scalar (cardset,bigword,value)
        call pc_put_gui (keyword, action, value)
      endif

      return
      end subroutine cfegui_modify_field


!!----------------------- modify_array_element ----------------------------!!
!!----------------------- modify_array_element ----------------------------!!
!!----------------------- modify_array_element ----------------------------!!


      subroutine cfegui_modify_array_element (window,keyword,value,indx)

      implicit none
      type(cfewindow_struct),pointer          :: window               !argument
      character(len=*),intent(in)             :: keyword              !argument
      character(len=*),intent(in)             :: value                !argument
      integer         ,intent(in)             :: indx                 !argument

      type(cardset_struct),pointer            :: cardset              !local
      character(len=PC_LENGTH) ,allocatable   :: temp_array(:)        !local
      character(len=PC_LENGTH) ,pointer       :: array(:)             !local
      integer                                 :: narray               !local
      character(len=PC_LENGTH) ,pointer       :: arrayset(:)          !local
      integer                                 :: narrayset            !local
      character(len=PC_LENGTH)                :: errmsg               !local
      character(len=PC_LENGTH)                :: bigword              !local
      character(len=PC_LENGTH)                :: local_keyword        !local
      character(len=PC_LENGTH)                :: extrarow             !local
      character(len=PC_LENGTH)                :: arrayset_keyword     !local
      integer                                 :: i                    !local

      if (.not. associated(window)) return

      nullify (cardset) ! jpa
      call cfewindow_get_cardset (window,cardset)

      bigword = trim(keyword)//'#REPLACEELEMENTS'
      if (cardset_keyword_present(cardset,bigword)) then
        nullify(array)
        nullify(arrayset)
        call cardset_alloc_array (cardset,bigword,array,narray,errmsg)
        if (indx .lt. 1 .or. indx .gt. narray) then
          call pc_error ('Index out of range for keyword '//trim(keyword))
          return
        endif
        extrarow = trim(keyword)//'#EXTRAROW'
        array(indx) = value
        if (cardset_keyword_present(cardset,extrarow)) then
          if (indx .eq. narray) then
            call cfegui_get_arrayset_name (keyword,arrayset_keyword,cardset,  &
                                           '#ARRAYSETELEMENTS')
            if (len_trim(arrayset_keyword) .gt. 0) then
              narrayset = 0
              call cardset_alloc_array (cardset,arrayset_keyword,arrayset,  &
                                        narrayset,errmsg)
            else
              narrayset = 1
              local_keyword = keyword
            endif
            do i=1,narrayset
              if (associated(arrayset)) local_keyword = arrayset(i)
              bigword   = trim(local_keyword)//'#REPLACEELEMENTS'
              extrarow = trim(local_keyword)//'#EXTRAROW'
              if (cardset_keyword_present(cardset,bigword)) then
                narray = 0
                call cardset_alloc_array (cardset,bigword,array,narray,errmsg)
                if (cardset_keyword_present(cardset,extrarow)) narray = narray-1
                if (indx .lt. 0 .or. indx .gt. narray+1) then
                  call pc_error ('Index out of range for keyword '  &
                                  //trim(local_keyword))
                  return
                endif
                if (narray .eq. 0) then
                  narray = 1
                  allocate(array(narray))
                  if (trim(local_keyword) .eq. trim(keyword)) then
                    array(1) = value
                  else
                    array(1) = ' '
                  endif
                else
                  allocate(temp_array(narray))
                  temp_array(1:narray) = array(1:narray)
                  deallocate(array)
                  allocate(array(narray+1))
                  array(1:indx-1) = temp_array(1:indx-1)
                  if (trim(local_keyword) .eq. trim(keyword)) then
                    array(indx) = value
                  else
                    array(indx) = ' '
                  endif
                  if (indx.le.narray) array(indx+1:narray+1) =  &
                                                         temp_array(indx:narray)
                  narray = narray + 1
                  deallocate(temp_array)
                endif
                call cardset_remove_keyword (cardset,extrarow)
                call cardset_put_array      (cardset,bigword,array,narray)
                call pc_put_gui (local_keyword, 'InsertIndex  '  , indx)
                call pc_put_gui (local_keyword, 'ReplaceElements',array,narray)
              endif
            enddo
          else
            call cardset_put_array (cardset,bigword,array,narray)
            call pc_put_gui (keyword, 'ModifyIndex', indx)
            call pc_put_gui (keyword, 'ReplaceElements' , array, narray-1)
          endif
        else
          call cardset_put_array (cardset,bigword,array,narray)
          call pc_put_gui (keyword, 'ModifyIndex', indx)
          call pc_put_gui (keyword, 'ReplaceElements' , array, narray)
        endif
        if (associated(array)) deallocate(array)
        if (associated(array)) deallocate(arrayset)
      endif

      return
      end subroutine cfegui_modify_array_element


!!----------------------- insert_array_element ----------------------------!!
!!----------------------- insert_array_element ----------------------------!!
!!----------------------- insert_array_element ----------------------------!!


      subroutine cfegui_insert_array_element (window,keyword,indx)

      implicit none
      type(cfewindow_struct),pointer          :: window               !argument
      character(len=*),intent(in)             :: keyword              !argument
      integer         ,intent(in)             :: indx                 !argument

      type(cardset_struct),pointer            :: cardset              !local
      character(len=PC_LENGTH) ,allocatable   :: temp_array(:)        !local
      character(len=PC_LENGTH) ,pointer       :: array(:)             !local
      integer                                 :: narray               !local
      character(len=PC_LENGTH) ,pointer       :: arrayset(:)          !local
      integer                                 :: narrayset            !local
      character(len=PC_LENGTH)                :: errmsg               !local
      character(len=PC_LENGTH)                :: local_keyword        !local
      character(len=PC_LENGTH)                :: bigword              !local
      character(len=PC_LENGTH)                :: extrarow             !local
      character(len=PC_LENGTH)                :: lastremoved          !local
      integer                                 :: i                    !local

      if (.not. associated(window)) return

      local_keyword = keyword
      nullify (cardset) ! jpa
      call cfewindow_get_cardset (window,cardset)

      bigword = trim(local_keyword)//'#ARRAYSETELEMENTS'
      if (cardset_keyword_present(cardset,bigword)) then
        nullify(arrayset)
        call cardset_alloc_array (cardset,bigword,arrayset,narrayset,  &
                                  errmsg)
      else
        narrayset = 1
      endif

      do i=1,narrayset
        if (associated(arrayset)) local_keyword = arrayset(i)
        bigword   = trim(local_keyword)//'#REPLACEELEMENTS'
        extrarow = trim(local_keyword)//'#EXTRAROW'
        if (cardset_keyword_present(cardset,bigword)) then
          nullify(array)
          narray = 0
          call cardset_alloc_array (cardset,bigword,array,narray,errmsg)
          if (cardset_keyword_present(cardset,extrarow)) narray = narray - 1
  !       if (indx .eq. narray+1) then
  !         call pc_error ('Extra empty row already exist')
  !         return
  !       endif
          if (indx .lt. 1 .or. indx .gt. narray+1) then
            call pc_error ('Index',indx,'out of range for keyword '  &
                            //local_keyword)
            return
          endif
  !       if (indx .lt. 1 .or. indx .gt. narray) then
  !         call pc_error ('Index out of range for keyword '  &
  !                         //trim(local_keyword))
  !         return
  !       endif
          if (narray .eq. 0) then
            narray = 1
            allocate(array(narray))
            lastremoved = trim(local_keyword)//'#LASTREMOVED'
            if (cardset_keyword_present(cardset,lastremoved)) then
              call cardset_get_scalar(cardset,lastremoved,array(1),errmsg)
            else
              array(1) = ' '
            endif
          else
            allocate(temp_array(narray))
            temp_array(1:narray) = array(1:narray)
            deallocate(array)
            allocate(array(narray+1))
            array(1:indx-1) = temp_array(1:indx-1)
            lastremoved = trim(local_keyword)//'#LASTREMOVED'
            if (cardset_keyword_present(cardset,lastremoved)) then
              call cardset_get_scalar(cardset,lastremoved,array(indx),  &
                                      errmsg)
            else
              array(indx) = ' '
            endif
            if (indx.le.narray) array(indx+1:narray+1)=temp_array(indx:narray)
            narray = narray + 1
            deallocate(temp_array)
          endif
          call pc_put_gui (local_keyword, 'InsertIndex'  , indx)
          call pc_put_gui (local_keyword, 'ReplaceElements' , array, narray)
        endif
      enddo
      if (associated(array))    deallocate(array)
      if (associated(arrayset)) deallocate(arrayset)

      return
      end subroutine cfegui_insert_array_element


!!----------------------- remove_array_element ----------------------------!!
!!----------------------- remove_array_element ----------------------------!!
!!----------------------- remove_array_element ----------------------------!!


      subroutine cfegui_remove_array_element (window,keyword,indx)

      implicit none
      type(cfewindow_struct),pointer          :: window               !argument
      character(len=*),intent(in)             :: keyword              !argument
      integer         ,intent(in)             :: indx                 !argument

      type(cardset_struct),pointer            :: cardset              !local
      character(len=PC_LENGTH) ,allocatable   :: temp_array(:)        !local
      character(len=PC_LENGTH) ,pointer       :: array(:)             !local
      integer                                 :: narray               !local
      character(len=PC_LENGTH) ,pointer       :: arrayset(:)          !local
      integer                                 :: narrayset            !local
      character(len=PC_LENGTH)                :: errmsg               !local
      character(len=PC_LENGTH)                :: local_keyword        !local
      character(len=PC_LENGTH)                :: bigword              !local
      character(len=PC_LENGTH)                :: extrarow             !local
      character(len=PC_LENGTH)                :: lastremoved          !local
      integer                                 :: i                    !local

      if (.not. associated(window)) return

      local_keyword = keyword

      nullify (cardset) ! jpa
      call cfewindow_get_cardset (window,cardset)

      bigword = trim(local_keyword)//'#ARRAYSETELEMENTS'
      if (cardset_keyword_present(cardset,bigword)) then
        nullify(arrayset)
        call cardset_alloc_array (cardset,bigword,arrayset,narrayset,  &
                                  errmsg)
      else
        narrayset = 1
      endif

      do i=1,narrayset
        if (associated(arrayset)) local_keyword = arrayset(i)
        bigword = trim(local_keyword)//'#REPLACEELEMENTS'
        extrarow = trim(local_keyword)//'#EXTRAROW'
        if (cardset_keyword_present(cardset,bigword)) then
          nullify(array)
          narray = 0
          call cardset_alloc_array (cardset,bigword,array,narray,errmsg)
          if (cardset_keyword_present(cardset,extrarow)) narray = narray - 1
          if (indx .eq. narray+1) then
            call pc_error ('Extra empty row may not be deleted')
            return
          endif
          if (indx .lt. 1 .or. indx .gt. narray) then
            call pc_error ('Index out of range for keyword ' //  &
                            trim(local_keyword))
            return
          endif
          allocate(temp_array(narray))
          temp_array(1:narray) = array(1:narray)
          deallocate(array)
          allocate(array(narray-1))
          if (indx .gt. 1) then
            array(1:indx-1) = temp_array(1:indx-1)
          endif
          lastremoved = trim(local_keyword)//'#LASTREMOVED'
          call cardset_put_scalar(cardset,lastremoved,temp_array(indx))
          array(indx:narray-1) = temp_array(indx+1:narray)
          narray = narray - 1
          deallocate(temp_array)
          call pc_put_gui (local_keyword, 'RemoveIndex'  , indx)
          call pc_put_gui (local_keyword, 'ReplaceElements' , array, narray)
        endif
      enddo
      if (associated(array))    deallocate(array)
      if (associated(arrayset)) deallocate(arrayset)

      return
      end subroutine cfegui_remove_array_element


!!---------------------- save_arrayset_elements ---------------------------!!
!!---------------------- save_arrayset_elements ---------------------------!!
!!---------------------- save_arrayset_elements ---------------------------!!


      subroutine cfegui_save_arrayset_elements (window,keyword,array,narray)

      implicit none
      type(cfewindow_struct),pointer          :: window               !argument
      character(len=*),intent(in)             :: keyword              !argument
      character(len=*),intent(in)             :: array(:)             !argument
      integer         ,intent(in)             :: narray               !argument

      type(cardset_struct),pointer            :: cardset              !local
      character(len=PC_LENGTH)                :: bigword              !local

      if (.not. associated(window)) return

      nullify (cardset) ! jpa
      call cfewindow_get_cardset (window,cardset)

      bigword = trim(keyword)//'#ARRAYSETELEMENTS'
      call cardset_put_array (cardset,bigword,array,narray)

      return
      end subroutine cfegui_save_arrayset_elements


!!---------------------- alloc_refresh_keywords ---------------------------!!
!!---------------------- alloc_refresh_keywords ---------------------------!!
!!---------------------- alloc_refresh_keywords ---------------------------!!


      subroutine cfegui_alloc_refresh_keywords (array,narray)

      implicit none
      character(len=*),pointer                 :: array(:)             !argument
      integer         ,intent(out)             :: narray               !argument

      integer                                  :: i                    !local

      integer                                  :: num_keywords         !local
      character(len=PC_LENGTH)                 :: action               !local
      character(len=PC_LENGTH)                 :: keyword              !local
      character(len=PC_LENGTH) ,allocatable    :: temp_array(:)        !local

      if (associated(array)) deallocate(array)
      narray = 0

      num_keywords = pc_num_gui_keywords()
      do i=1,num_keywords
        keyword = pc_get_gui_keyword(i)
        action = pc_get_gui_action(i)
        if (trim(action) .ne. 'CLEARSELECTION') cycle
        if (associated(array)) then
          allocate(temp_array(narray))
          temp_array(1:narray) = array(1:narray)
          deallocate(array)
          allocate(array(narray+1))
          array(1:narray) = temp_array(1:narray)
          deallocate(temp_array)
        else
          allocate(array(narray+1))
        endif
        narray = narray + 1
        array(narray) = trim(keyword)
!       call string_to_upper (array(narray))
      enddo

      return
      end subroutine cfegui_alloc_refresh_keywords


!!------------------------- get_arrayset_name -----------------------------!!
!!------------------------- get_arrayset_name -----------------------------!!
!!------------------------- get_arrayset_name -----------------------------!!


      subroutine cfegui_get_arrayset_name (keyword,arrayset,cardset,use_action)

      implicit none
      character(len=*),intent(in)              :: keyword              !argument
      character(len=*),intent(out)             :: arrayset             !argument
      type(cardset_struct),pointer,optional    :: cardset              !argument
      character(len=*),optional                :: use_action           !argument

      integer                                  :: i                    !local
      integer                                  :: j                    !local
      integer                                  :: k                    !local
      integer                                  :: mode                 !local
      integer                                  :: num_keywords         !local
      integer                                  :: nature               !local
      integer                                  :: narray               !local
      character(len=PC_LENGTH)                 :: local_keyword        !local
      character(len=PC_LENGTH)                 :: err                  !local
      character(len=PC_LENGTH)                 :: action               !local
      character(len=PC_LENGTH)                 :: compare_action       !local
      character(len=PC_LENGTH),pointer         :: array(:)             !local

      arrayset = ' '
      nullify(array)

      mode = 1
      if (present(cardset)) mode = 2
      if (present(use_action)) then
        compare_action = use_action
      else
        compare_action = '#ARRAYNAMES'
      endif

      select case (mode)
        case (1)                                      ! from parameter cache
          num_keywords = pc_num_gui_keywords()
          do i=1,num_keywords
            local_keyword = pc_get_gui_keyword(i)
            action = pc_get_gui_action(i)
            if (trim(action) .ne. 'ARRAYNAMES') cycle
            nature  = pc_nature_gui(local_keyword,trim(action))
            if (nature .eq. PC_ARRAY) then
              narray = pc_num_elements_gui(local_keyword,trim(action))
              if (narray .gt. 0) then
                call pc_alloc_gui(local_keyword,trim(action),array,narray)
                do j=1,narray
                  call string_to_upper (array(j))
                  if (trim(array(j)) .eq. trim(keyword)) then
                    arrayset = local_keyword
                    exit
                  endif
                enddo
              endif
            endif
          enddo
        case (2)                                      ! from cardset 
          num_keywords = cardset_num_keywords(cardset)
          do i=1,num_keywords
            local_keyword = cardset_get_keyword(cardset,i)
            k = index(local_keyword,trim(compare_action),.true.)
            if (k .eq. 0) cycle
            nature  = cardset_nature(cardset,local_keyword)
            if (nature .eq. PC_ARRAY) then
              narray = cardset_num_elements(cardset,local_keyword)
              if (narray .gt. 0) then
                call cardset_alloc_array(cardset,local_keyword,array,narray,err)
                do j=1,narray
                  call string_to_upper (array(j))
                  if (trim(array(j)) .eq. trim(keyword)) then
                    arrayset = local_keyword
                    exit
                  endif
                enddo
              endif
            endif
          enddo
      end select

      if (associated(array)) deallocate(array)

      return
      end subroutine cfegui_get_arrayset_name


!!------------------------- get_array_minsize -----------------------------!!
!!------------------------- get_array_minsize -----------------------------!!
!!------------------------- get_array_minsize -----------------------------!!


      subroutine cfegui_get_array_minsize (keyword,minsize,cardset)

      implicit none
      character(len=*),intent(in)              :: keyword              !argument
      integer         ,intent(out)             :: minsize              !argument
      type(cardset_struct),pointer,optional    :: cardset              !argument

      integer                                  :: mode                 !local
      character(len=PC_LENGTH)                 :: cminsize             !local
      character(len=PC_LENGTH)                 :: bigword              !local
      character(len=PC_LENGTH)                 :: arrayset             !local
      character(len=PC_LENGTH)                 :: errmsg               !local

      minsize = NO_MINSIZE

      mode = 1
      if (present(cardset)) mode = 2

      select case (mode)
        case (1)                                      ! from parameter cache
          if (pc_gui_action_present(keyword,'MINSIZEARRAYSET')) then
            call pc_get_gui (keyword,'MINSIZEARRAYSET',minsize)
          else
            call cfegui_get_arrayset_name (keyword,arrayset)
            if (len_trim(arrayset) .gt. 0) then
              if (pc_gui_action_present(arrayset,'MINSIZEARRAYSET')) then
                call pc_get_gui (arrayset,'MINSIZEARRAYSET',minsize)
              else if (pc_gui_action_present(keyword,'MINSIZEARRAY')) then
                call pc_get_gui (keyword,'MINSIZEARRAY',minsize)
              endif
            else if (pc_gui_action_present(keyword,'MINSIZEARRAY')) then
              call pc_get_gui (keyword,'MINSIZEARRAY',minsize)
            endif
          endif
        case (2)                                      ! from cardset
          bigword = trim(keyword)//'#MINSIZEARRAYSET'
          if (cardset_keyword_present(cardset,bigword)) then
            call cardset_get_scalar (cardset,bigword,cminsize,errmsg)
            call string_cc2ii (cminsize,minsize)
          else
            call cfegui_get_arrayset_name (keyword,arrayset,cardset)
            bigword = trim(keyword)//'#MINSIZEARRAY'
            if (len_trim(arrayset) .gt. 0) then
              bigword = trim(arrayset)//'#MINSIZEARRAYSET'
              if (cardset_keyword_present(cardset,bigword)) then
                call cardset_get_scalar (cardset,bigword,cminsize,errmsg)
                call string_cc2ii (cminsize,minsize)
              else 
                bigword = trim(keyword)//'#MINSIZEARRAY'
                if (cardset_keyword_present(cardset,bigword)) then
                  call cardset_get_scalar (cardset,bigword,cminsize,errmsg)
                  call string_cc2ii (cminsize,minsize)
                endif
              endif
            else if (cardset_keyword_present(cardset,bigword)) then
              call cardset_get_scalar (cardset,bigword,cminsize,errmsg)
              call string_cc2ii (cminsize,minsize)
            endif
          endif
      end select

      if (minsize .lt. 0) minsize = NO_MINSIZE

      return
      end subroutine cfegui_get_array_minsize


!!------------------------- get_array_maxsize -----------------------------!!
!!------------------------- get_array_maxsize -----------------------------!!
!!------------------------- get_array_maxsize -----------------------------!!


      subroutine cfegui_get_array_maxsize (keyword,maxsize,cardset)

      implicit none
      character(len=*),intent(in)              :: keyword              !argument
      integer         ,intent(out)             :: maxsize              !argument
      type(cardset_struct),pointer,optional    :: cardset              !argument

      integer                                  :: mode                 !local
      character(len=PC_LENGTH)                 :: cmaxsize             !local
      character(len=PC_LENGTH)                 :: bigword              !local
      character(len=PC_LENGTH)                 :: arrayset             !local
      character(len=PC_LENGTH)                 :: errmsg               !local

      maxsize = NO_MAXSIZE

      mode = 1
      if (present(cardset)) mode = 2

      select case (mode)
        case (1)                                      ! from parameter cache
          if (pc_gui_action_present(keyword,'MAXSIZEARRAYSET')) then
            call pc_get_gui (keyword,'MAXSIZEARRAYSET',maxsize)
          else
            call cfegui_get_arrayset_name (keyword,arrayset)
            if (len_trim(arrayset) .gt. 0) then
              if (pc_gui_action_present(arrayset,'MAXSIZEARRAYSET')) then
                call pc_get_gui (arrayset,'MAXSIZEARRAYSET',maxsize)
              else if (pc_gui_action_present(keyword,'MAXSIZEARRAY')) then
                call pc_get_gui (keyword,'MAXSIZEARRAY',maxsize)
              endif
            else if (pc_gui_action_present(keyword,'MAXSIZEARRAY')) then
              call pc_get_gui (keyword,'MAXSIZEARRAY',maxsize)
            endif
          endif
        case (2)                                      ! from cardset
          bigword = trim(keyword)//'#MAXSIZEARRAYSET'
          if (cardset_keyword_present(cardset,bigword)) then
            call cardset_get_scalar (cardset,bigword,cmaxsize,errmsg)
            call string_cc2ii (cmaxsize,maxsize)
          else
            call cfegui_get_arrayset_name (keyword,arrayset,cardset)
            bigword = trim(keyword)//'#MAXSIZEARRAY'
            if (len_trim(arrayset) .gt. 0) then
              bigword = trim(arrayset)//'#MAXSIZEARRAYSET'
              if (cardset_keyword_present(cardset,bigword)) then
                call cardset_get_scalar (cardset,bigword,cmaxsize,errmsg)
                call string_cc2ii (cmaxsize,maxsize)
              else 
                bigword = trim(keyword)//'#MAXSIZEARRAY'
                if (cardset_keyword_present(cardset,bigword)) then
                  call cardset_get_scalar (cardset,bigword,cmaxsize,errmsg)
                  call string_cc2ii (cmaxsize,maxsize)
                endif
              endif
            else if (cardset_keyword_present(cardset,bigword)) then
              call cardset_get_scalar (cardset,bigword,cmaxsize,errmsg)
              call string_cc2ii (cmaxsize,maxsize)
            endif
          endif
      end select

      if (maxsize .lt. 0) maxsize = NO_MAXSIZE

      return
      end subroutine cfegui_get_array_maxsize


!!------------------------- remove_array_size -----------------------------!!
!!------------------------- remove_array_size -----------------------------!!
!!------------------------- remove_array_size -----------------------------!!


      subroutine cfegui_remove_array_size (keyword,cardset)

      implicit none
      character(len=*),intent(in)              :: keyword              !argument
      type(cardset_struct),pointer             :: cardset              !argument

      character(len=PC_LENGTH)                 :: bigword              !local
      character(len=PC_LENGTH)                 :: arrayset             !local

      if (.not. pc_gui_action_present(keyword,'MINSIZEARRAY')) then
        bigword = trim(keyword) // '#MINSIZEARRAY'
        call cardset_remove_keyword (cardset,bigword)
      endif
      if (.not. pc_gui_action_present(keyword,'MAXSIZEARRAY')) then
        bigword = trim(keyword) // '#MAXSIZEARRAY'
        call cardset_remove_keyword (cardset,bigword)
      endif
      call cfegui_get_arrayset_name (keyword,arrayset,cardset)
      if (len_trim(arrayset) .ne. 0) then
        if (.not. pc_gui_action_present(arrayset,'ARRAYNAMES')) then
          bigword = trim(arrayset) // '#ARRAYNAMES'
          call cardset_remove_keyword (cardset,bigword)
        endif
        if (.not. pc_gui_action_present(arrayset,'MINSIZEARRAYSET')) then
          bigword = trim(arrayset) // '#MINSIZEARRAYSET'
          call cardset_remove_keyword (cardset,bigword)
        endif
        if (.not. pc_gui_action_present(arrayset,'MAXSIZEARRAYSET')) then
          bigword = trim(arrayset) // '#MAXSIZEARRAYSET'
          call cardset_remove_keyword (cardset,bigword)
        endif
      endif

      return
      end subroutine cfegui_remove_array_size


!!--------------------------- insert_allowed ------------------------------!!
!!--------------------------- insert_allowed ------------------------------!!
!!--------------------------- insert_allowed ------------------------------!!


      function cfegui_insert_allowed (window,keyword) result (allowed)

      implicit none
      type(cfewindow_struct),pointer          :: window               !argument
      character(len=*),intent(in)              :: keyword              !argument
      logical                                  :: allowed              !result

      integer                                  :: i                    !local
      integer                                  :: maxsize              !local
      integer                                  :: narray               !local
      integer                                  :: narrayset            !local
      character(len=PC_LENGTH),pointer         :: arrayset(:)          !local
      character(len=PC_LENGTH)                 :: bigword              !local
      character(len=PC_LENGTH)                 :: cmaxsize             !local
      character(len=PC_LENGTH)                 :: errmsg               !local
      type(cardset_struct),pointer             :: cardset              !local

      allowed = .true.

      nullify (cardset) ! jpa
      call cfewindow_get_cardset (window,cardset)
      bigword = trim(keyword)//'#ARRAYNAMES'
      if (cardset_keyword_present(cardset,bigword)) then
        bigword = trim(keyword)//'#MAXSIZEARRAYSET'
        if (cardset_keyword_present(cardset,bigword)) then
          call cardset_get_scalar (cardset,bigword,cmaxsize,errmsg)
          call string_cc2ii (cmaxsize,maxsize)
          nullify(arrayset)
          bigword = trim(keyword)//'#ARRAYNAMES'
          narrayset = 0
          call cardset_alloc_array (cardset,bigword,arrayset,narrayset,errmsg)
          if (narrayset .gt. 0) then
            do i=1,narrayset
              bigword = trim(arrayset(i))//'#REPLACEELEMENTS'
              if (cardset_keyword_present(cardset,bigword)) then
                narray = 0
                narray = cardset_num_elements (cardset,bigword)
                if (narray-1 .lt. maxsize) allowed = .false.
                exit
              endif
            enddo
          endif
          if (associated(arrayset)) deallocate(arrayset)
        else
          nullify(arrayset)
          bigword = trim(keyword)//'#ARRAYNAMES'
          narrayset = 0
          call cardset_alloc_array (cardset,bigword,arrayset,narrayset,errmsg)
          if (narrayset .gt. 0) then
            maxsize = NO_MAXSIZE
            do i=1,narrayset
              bigword = trim(arrayset(i))//'#MAXSIZEARRAY'
              if (cardset_keyword_present(cardset,bigword)) then
                call cardset_get_scalar (cardset,bigword,cmaxsize,errmsg)
                call string_cc2ii (cmaxsize,maxsize)
                exit
              endif
            enddo
          endif
          if (associated(arrayset)) deallocate(arrayset)
          bigword = trim(keyword)//'#ARRAYNAMES'
          narrayset = 0
          call cardset_alloc_array (cardset,bigword,arrayset,narrayset,errmsg)
          if (narrayset .gt. 0) then
            do i=1,narrayset
              bigword = trim(arrayset(i))//'#REPLACEELEMENTS'
              if (cardset_keyword_present(cardset,bigword)) then
                narray = 0
                narray = cardset_num_elements (cardset,bigword)
                if (narray-1 .lt. maxsize) allowed = .false.
                exit
              endif
            enddo
          endif
          if (associated(arrayset)) deallocate(arrayset)
        endif

      else
        call cfegui_get_array_maxsize (keyword,maxsize,cardset)
        if (maxsize .ne. NO_MAXSIZE) then
          bigword = trim(keyword)//'#REPLACEELEMENTS'
          if (cardset_keyword_present(cardset,bigword)) then
            narray = 0
            narray = cardset_num_elements (cardset,bigword)
            if (narray+1 .gt. maxsize) allowed = .false.
          endif
        endif
      endif

      return
      end function cfegui_insert_allowed


!!--------------------------- remove_allowed ------------------------------!!
!!--------------------------- remove_allowed ------------------------------!!
!!--------------------------- remove_allowed ------------------------------!!


      function cfegui_remove_allowed (window,keyword) result (allowed)

      implicit none
      type(cfewindow_struct),pointer          :: window               !argument
      character(len=*),intent(in)             :: keyword              !argument
      logical                                 :: allowed              !result

      integer                                 :: i                    !local
      integer                                 :: minsize              !local
      integer                                 :: narray               !local
      integer                                 :: narrayset            !local
      character(len=PC_LENGTH),pointer        :: arrayset(:)          !local
      character(len=PC_LENGTH)                :: bigword              !local
      character(len=PC_LENGTH)                :: cminsize             !local
      character(len=PC_LENGTH)                :: errmsg               !local
      type(cardset_struct),pointer            :: cardset              !local

      allowed = .true.

      nullify (cardset) ! jpa
      call cfewindow_get_cardset (window,cardset)
      bigword = trim(keyword)//'#ARRAYNAMES'
      if (cardset_keyword_present(cardset,bigword)) then
        bigword = trim(keyword)//'#MINSIZEARRAYSET'
        if (cardset_keyword_present(cardset,bigword)) then
          call cardset_get_scalar (cardset,bigword,cminsize,errmsg)
          call string_cc2ii (cminsize,minsize)
          nullify(arrayset)
          bigword = trim(keyword)//'#ARRAYNAMES'
          narrayset = 0
          call cardset_alloc_array (cardset,bigword,arrayset,narrayset,errmsg)
          if (narrayset .gt. 0) then
            do i=1,narrayset
              bigword = trim(arrayset(i))//'#REPLACEELEMENTS'
              if (cardset_keyword_present(cardset,bigword)) then
                narray = 0
                narray = cardset_num_elements (cardset,bigword)
                if (narray-1 .lt. minsize) allowed = .false.
                exit
              endif
            enddo
          endif
          if (associated(arrayset)) deallocate(arrayset)
        else
          nullify(arrayset)
          bigword = trim(keyword)//'#ARRAYNAMES'
          narrayset = 0
          call cardset_alloc_array (cardset,bigword,arrayset,narrayset,errmsg)
          if (narrayset .gt. 0) then
            minsize = NO_MINSIZE
            do i=1,narrayset
              bigword = trim(arrayset(i))//'#MINSIZEARRAY'
              if (cardset_keyword_present(cardset,bigword)) then
                call cardset_get_scalar (cardset,bigword,cminsize,errmsg)
                call string_cc2ii (cminsize,minsize)
                exit
              endif
            enddo
          endif
          if (associated(arrayset)) deallocate(arrayset)
          bigword = trim(keyword)//'#ARRAYNAMES'
          narrayset = 0
          call cardset_alloc_array (cardset,bigword,arrayset,narrayset,errmsg)
          if (narrayset .gt. 0) then
            do i=1,narrayset
              bigword = trim(arrayset(i))//'#REPLACEELEMENTS'
              if (cardset_keyword_present(cardset,bigword)) then
                narray = 0
                narray = cardset_num_elements (cardset,bigword)
                if (narray-1 .lt. minsize) allowed = .false.
                exit
              endif
            enddo
          endif
          if (associated(arrayset)) deallocate(arrayset)
        endif

      else
        call cfegui_get_array_minsize (keyword,minsize,cardset)
        if (minsize .ne. NO_MINSIZE) then
          bigword = trim(keyword)//'#REPLACEELEMENTS'
          if (cardset_keyword_present(cardset,bigword)) then
            narray = 0
            narray = cardset_num_elements (cardset,bigword)
            if (narray-1 .lt. minsize) allowed = .false.
          endif
        endif
      endif

      return
      end function cfegui_remove_allowed


!!-------------------------- keyword_matches ------------------------------!!
!!-------------------------- keyword_matches ------------------------------!!
!!-------------------------- keyword_matches ------------------------------!!


      function cfegui_keyword_matches (keyword,array,narray) result (matches)

      implicit none
      character(len=*),intent(in)              :: keyword              !argument
      character(len=*),intent(in)              :: array(:)             !argument
      integer         ,intent(in)              :: narray               !argument
      logical                                  :: matches              !result

      integer                                  :: i                    !local
      matches = .false.

      if (narray .gt. 0) then
        do i=1,narray
          if (trim(array(i)) .eq. trim(keyword)) then
            matches = .true.
            exit
          endif
        enddo
      endif

      return
      end function cfegui_keyword_matches


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module cfegui_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

