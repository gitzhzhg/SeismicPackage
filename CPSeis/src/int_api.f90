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
! Name       : int_api 
! Category   : cfe
! Written    : 1999-08-11   by: Donna K. Vunderink
! Revised    : 2007-10-04   by: Karen Goodger
! Maturity   : beta
! Purpose    : API Interface functions.
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
! 16. 2007-10-04  Goodger      Additional printout when unable to open pc_lun.
! 15. 2006-12-04  D. Glover    Added NULLIFY statements for Intel compiler
! 14. 2006-06-20  B. Menger    Removed Unused Variables.
! 13. 2004-05-26  SMCook       Added MISCELLANEOUSPARAMETER action as means of
!                               passing more parameters to the back end.  So far
!                               used only for new CUSTOMPATH keyword, whose
!                               value should point to the platform directory
!                               containing the icps executable for the custom
!                               code case (among other things).
!                              Goes along with a CFE Java code update.
! 12. 2003-09-15  Stoeckley    Changed changed type to primitive; changed
!                               category to cfe; changed names of called
!                               primitives as necessary.
! 11. 2002-09-23  Vunderink    In case they return an error, add iostat to
!                                rewind and endfile.  Changed prints to writes
!                                for checkc.
! 10. 2002-08-15  Vunderink    Added close of scratch file unit pc_lun when
!                                exit button pressed.
!  9. 2002-07-15  Vunderink    Added close of scratch file unit pc_lun when
!                                terminating application.
!  8. 2000-10-11  Vunderink    Modified to save last keyword and action and
!                                pass to cfegui_modify_field.  This is to get
!                                around INT software problem of sending extra
!                                MODIFYFIELD actions.
!  7. 2000-09-04  Vunderink    Do not pass on EnterScreen action to processes.
!  6. 2000-08-15  Vunderink    Changed name from cfe_api to int_api
!  5. 2000-05-26  Vunderink    Added pc_put_gui call to pass execution mode.
!  4. 2000-05-23  Vunderink    Added cfegui calls to check if INSERTELEMENT or
!                                REMOVEELEMENT are allowed on the array.
!  3. 2000-04-24  Vunderink    Changed character variables to use named constant
!                                CFE_LENGTH, and added support for EnterWindow
!                                action (if window is type CFE_MAIN)
!  2. 2000-03-02  Vunderink    Added call to pc_gui_update and pc_restore around
!                                cfe_delete
!  1. 1999-08-11  Vunderink    Initial version.
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


      module int_api_module

      use cfestruct_module
      use pc_module
      implicit none

      character(len=100),public,save :: int_api_ident = &
       '$Id: int_api.f90,v 1.16 2007/10/05 14:42:34 Goodger beta sps $'

      type(cfestruct),public,pointer,save             :: obj
      integer         ,public       ,save             :: pc_lun       = 6
      character(len=PC_LENGTH)      ,save             :: last_action  = ' '
      character(len=PC_LENGTH)      ,save             :: last_keyword = ' '


!!---------------------------- end of module ------------------------------!!
!!---------------------------- end of module ------------------------------!!
!!---------------------------- end of module ------------------------------!!


      end module int_api_module


!!-------------------------- int_api_set_value ----------------------------!!
!!-------------------------- int_api_set_value ----------------------------!!
!!-------------------------- int_api_set_value ----------------------------!!


      subroutine int_api_set_value (id, h_action, h_keyword, h_value)

      use cfe_module
      use int_api_module
      use cfegui_module
      use cfestruct_module
      use cfewindow_module
      use pc_module
      use string_module
      use getlun_module
      use pclun_module

      implicit none

      integer,intent(in)              :: id                          !argument
      integer,intent(in)              :: h_action(*)                 !argument
      integer,intent(in)              :: h_keyword(*)                !argument
      integer,intent(in)              :: h_value(*)                  !argument

      character(len=PC_LENGTH)        :: action                      !local
      character(len=PC_LENGTH)        :: keyword                     !local
      character(len=PC_LENGTH)        :: value                       !local
      integer                         :: naction                     !local
      integer                         :: nkeyword                    !local

      integer                         :: ivalue                      !local
      integer                         :: istat                       !local
      type(cfewindow_struct),pointer  :: windowCurrent               !local
      character(len=PC_LENGTH)        :: windowKeyword               !local
      character(len=PC_LENGTH)        :: windowType                  !local

      nullify (windowCurrent) ! jpa

      call string_hh2cc (h_action , action)
      call string_hh2cc (h_keyword, keyword)
      call string_hh2cc (h_value  , value)

      call string_strip_blanks (action,naction)
      call string_to_upper (action(1:naction))


      if (action(1:naction).ne.'INITIALIZEAPP' .and. .not.associated(obj)) then
        return
      endif

      select case (action(1:naction))

        case('MISCELLANEOUSPARAMETER')
          if (trim(keyword) .eq. 'CUSTOMPATH') obj%custom_path = trim(value)

        case('BUTTONPRESS')
          call cfewindow_get_pointer (id,windowCurrent)
          call cfewindow_set_current (windowCurrent)
          call cfewindow_get_keyword (windowCurrent,windowKeyword)
          pc_lun = pclun_get()
          call pc_gui_update (pc_lun)
          call cfegui_modify_field (windowCurrent,keyword, action, value,  &
                                    last_keyword,last_action)
          call cfe_update (obj)
          call cfewindow_get_current (windowCurrent)
          call cfegui_new_window (windowCurrent)
          call cfewindow_get_current (windowCurrent)
          if (associated(obj)) call cfegui_transfer (windowCurrent)
          call pc_restore
          if (trim(keyword) .eq. 'EXIT') close (unit=pc_lun,iostat=istat)

        case('ITEMCLICKED','LEAVEARRAY','LEAVEARRAYSET','LEAVESCREEN',  &
             'MODIFYFIELD')
          call cfewindow_get_pointer (id,windowCurrent)
          call cfewindow_set_current (windowCurrent)
          call cfewindow_get_keyword (windowCurrent,windowKeyword)
          pc_lun = pclun_get()
          call pc_gui_update (pc_lun)
          call cfegui_modify_field (windowCurrent,keyword, action, value,  &
                                    last_keyword,last_action)
          call cfe_update (obj)
          call cfewindow_get_current (windowCurrent)
          if (associated(obj)) call cfegui_transfer (windowCurrent)
          call pc_restore

        case('CLOSEWINDOW')
          call cfewindow_get_pointer (id,windowCurrent)
          call cfewindow_set_current (windowCurrent)
          call cfegui_delete (id,'Close')
          call cfewindow_delete (windowCurrent)

        case('ENTERSCREEN','ENTERWINDOW')
          call cfewindow_get_pointer (id,windowCurrent)
          call cfewindow_get_window_type (windowCurrent,windowType)
          if (trim(windowType) .eq. 'MAIN') then                    !Main Window
            call cfewindow_set_current (windowCurrent)
            call cfewindow_get_keyword (windowCurrent,windowKeyword)
            pc_lun = pclun_get()
            call pc_gui_update (pc_lun)
            call cfegui_modify_field (windowCurrent,keyword, action, value,  &
                                       last_keyword,last_action)
            call cfe_update (obj)
            call cfewindow_get_current (windowCurrent)
            if (associated(obj)) call cfegui_transfer (windowCurrent)
            call pc_restore
          endif

        case('INITIALIZEAPP')
          call getlun (pc_lun,istat)
          open (unit=pc_lun,status='SCRATCH',access='SEQUENTIAL',  &
                form='FORMATTED',iostat=istat)
          if (istat .ne. 0) then
            call pc_error('Status on open = ',istat,' pc_lun = ',istat)
            call pc_error("Could not open pc_lun, Exit application")
          endif
          call pclun_set (pc_lun)
          call pc_gui_update (pc_lun)
          call pc_put_gui (action,'MODIFYFIELD',keyword)
          call cfe_create (obj)
          call cfewindow_get_current (windowCurrent)
          call cfewindow_get_window_id (windowCurrent,obj%mainWindowID)
          call cfegui_transfer (windowCurrent)
          call pc_restore

        case('INSERTELEMENT')
          call cfewindow_get_pointer (id,windowCurrent)
          call cfewindow_set_current (windowCurrent)
          call cfewindow_get_keyword (windowCurrent,windowKeyword)
          pc_lun = pclun_get()
          call pc_gui_update (pc_lun)
          call string_to_upper (keyword)
          call string_strip_blanks (keyword,nkeyword)
          call string_cc2ii (trim(value),ivalue)
          if (cfegui_insert_allowed(windowCurrent,keyword)) then
            call cfegui_insert_array_element (windowCurrent,keyword,ivalue)
            call cfe_update (obj)
          else
            call pc_error ('Insert not allowed')
          endif
          call cfewindow_get_current (windowCurrent)
          call cfegui_transfer (windowCurrent)
          call pc_restore

        case('REMOVEELEMENT')
          call cfewindow_get_pointer (id,windowCurrent)
          call cfewindow_set_current (windowCurrent)
          call cfewindow_get_keyword (windowCurrent,windowKeyword)
          pc_lun = pclun_get()
          call pc_gui_update (pc_lun)
          call string_to_upper (keyword)
          call string_strip_blanks (keyword,nkeyword)
          call string_cc2ii (trim(value),ivalue)
          if (cfegui_remove_allowed(windowCurrent,keyword)) then
            call cfegui_remove_array_element (windowCurrent,keyword,ivalue)
            call cfe_update (obj)
          else
            call pc_error('Remove not allowed')
          endif
          call cfewindow_get_current (windowCurrent)
          call cfegui_transfer (windowCurrent)
          call pc_restore

        case('REFRESHAPP')

        case('REFRESHWINDOW')

        case('TERMINATEAPP')
          pc_lun = pclun_get()
          call pc_gui_update (pc_lun)
          call cfe_delete (obj)
          call pc_restore
          close (unit=pc_lun,iostat=istat)

        case default
          write(STDOUT,*) '************* Unknown Action *************'
          write(STDOUT,*) 'action =',action
          write(STDOUT,*) 'keyword=',keyword
          write(STDOUT,*) 'value  =',value

      end select


      last_action  = action
      last_keyword = keyword

      rewind (pc_lun,iostat=istat)
      endfile (pc_lun,iostat=istat)
      rewind (pc_lun,iostat=istat)


      end subroutine int_api_set_value


!!-------------------------- int_api_set_array ----------------------------!!
!!-------------------------- int_api_set_array ----------------------------!!
!!-------------------------- int_api_set_array ----------------------------!!


      subroutine int_api_set_array(id,h_action,h_keyword,h_array ,start,end,siz)

      use cfe_module
      use int_api_module
      use cfegui_module
      use cfestruct_module
      use cfewindow_module
      use pc_module
      use string_module
      use pclun_module

      implicit none

      integer,intent(in)               :: id                          !argument
      integer,intent(in)               :: h_action(*)                 !argument
      integer,intent(in)               :: h_keyword(*)                !argument
      integer,intent(in)               :: h_array(*)                  !argument
      integer,intent(in)               :: start                       !argument
      integer,intent(in)               :: end                         !argument
      integer,intent(in)               :: siz                         !argument

      character(len=PC_LENGTH)         :: action                      !local
      character(len=PC_LENGTH)         :: keyword                     !local
      character(len=PC_LENGTH)         :: value                       !local
      character(len=PC_LENGTH) ,pointer :: array(:)                   !local
      integer                          :: naction                     !local
      integer                          :: nkeyword                    !local
      integer                          :: narray                      !local
      integer                          :: nbpw                        !local
      integer                          :: nwpe                        !local
      integer                          :: i                           !local
      integer                          :: j                           !local
      integer                          :: i1                          !local
      integer                          :: i2                          !local
      integer,allocatable              :: h_temp(:)                   !local
      type(cfewindow_struct),pointer  :: windowCurrent               !local
      character(len=PC_LENGTH)         :: windowKeyword               !local
      character(len=PC_LENGTH)         :: windowType                  !local

      nullify (windowCurrent) ! jpa

      call string_hh2cc (h_action , action)
      call string_hh2cc (h_keyword, keyword)
      nbpw   = bit_size(i) / 8                       ! # bytes per word
      nwpe   = siz/nbpw                              ! # words per array element
      narray = end-start+1
      allocate(array(narray))
      allocate(h_temp(nwpe+1))
      h_temp = 0
      do i=1,narray
        i1 = siz*(i-1)/nbpw+1
        i2 = i1+siz/nbpw-1
        do j=1,nwpe
          h_temp(j) = h_array(i1+j-1)
        enddo
        call string_hh2cc (h_temp , value)
        array(i) = value
      enddo
      deallocate(h_temp)

      call string_strip_blanks (action,naction)
      call string_to_upper (action(1:naction))

      select case (action(1:naction))

        case('ARRAYNAMES')
          call cfewindow_get_pointer (id,windowCurrent)
          call cfegui_save_arrayset_elements (windowCurrent,keyword,  &
                                               array,narray)

        case('MODIFYARRAYELEMENT')
          call cfewindow_get_pointer (id,windowCurrent)
          call cfewindow_set_current (windowCurrent)
          call cfewindow_get_keyword (windowCurrent,windowKeyword)
          call cfewindow_get_window_type (windowCurrent,windowType)
          if (trim(windowType) .eq. 'TEMPLATEPROCESS') then
            pc_lun = pclun_get()
            call pc_gui_update (pc_lun)
            call string_to_upper (keyword)
            call string_strip_blanks (keyword,nkeyword)
            call cfegui_modify_array_element (windowCurrent,keyword,  &
                                               array(1),start)
            call cfe_update (obj)
            call cfewindow_get_current (windowCurrent)
            call cfegui_transfer (windowCurrent)
            call pc_restore
          else
            pc_lun = pclun_get()
            call pc_gui_update (pc_lun)
            call string_to_upper (keyword)
            call string_strip_blanks (keyword,nkeyword)
            call cfegui_modify_array_element (windowCurrent,keyword,  &
                                               array(1),start)
            call cfe_update (obj)
            call cfewindow_get_current (windowCurrent)
            call cfegui_transfer (windowCurrent)
            call pc_restore
          endif

        case('ITEMSELECTED','PASTEELEMENTS')
          call cfewindow_get_pointer (id,windowCurrent)
          call cfewindow_set_current (windowCurrent)
          call cfewindow_get_keyword (windowCurrent,windowKeyword)
          pc_lun = pclun_get()
          call pc_gui_update (pc_lun)
          call string_to_upper (keyword)
          call string_strip_blanks (keyword,nkeyword)
          call pc_put_gui (keyword,trim(action),array,narray)
          call cfe_update (obj)
          call cfewindow_get_current (windowCurrent)
          call cfegui_transfer (windowCurrent)
          call pc_restore

        case default
          write(STDOUT,*) '************* Unknown Action *************'
          write(STDOUT,*) 'action =',action
          write(STDOUT,*) 'keyword=',keyword
          write(STDOUT,*) 'array  =',array
      end select

      deallocate(array)

      last_action  = action
      last_keyword = keyword

      end subroutine int_api_set_array



!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
